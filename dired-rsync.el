;;; dired-rsync.el --- Allow rsync from dired buffers -*- lexical-binding: t -*-
;;
;; Copyright (C) 2018, 2019, 2020 Alex Bennée
;;
;; Author: Alex Bennée <alex@bennee.com>
;; Maintainer: Alex Bennée <alex@bennee.com>
;; Version: 0.6
;; Package-Requires: ((s "1.12.0") (dash "2.0.0") (emacs "24"))
;; Homepage: https://github.com/stsquad/dired-rsync
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; dired-rsync is a command that can be run from a dired buffer to
;; copy files using rsync rather than tramps in-built mechanism.
;; This is especially useful for copying large files to/from remote
;; locations without locking up tramp.
;;
;; To use simply open a dired buffer, mark some files and invoke
;; dired-rsync.  After being prompted for a location to copy to an
;; inferior rsync process will be spawned.
;;
;; Wherever the files are selected from the rsync will always run from
;; your local machine.
;;

(require 'tramp) ; for tramp-tramp-file-p
(require 'dired-aux) ; for dired-dwim-target-directory
(require 'dash)
(require 's)
(require 'rx)

;;; Code:

;; Customisation options

(defcustom dired-rsync-command "rsync"
  "The rsync binary that we are going to use."
  :type 'string
  :group 'dired-rsync)

(defcustom dired-rsync-options "-az --info=progress2"
  "The default options for the rsync command."
  :type 'string
  :group 'dired-rsync)

(defcustom dired-rsync-unmark-on-completion t
  "Control if dired-rsync should unmark when complete."
  :type 'boolean
  :group 'dired-rsync)

(defun dired-rsync--default-fetch-marked-files ()
  "Default fetcher of marked files."
  (dired-get-marked-files nil current-prefix-arg))

(defcustom dired-rsync-source-files 'dired-rsync--default-fetch-marked-files
  "Function to collect the list of source files from dired."
  :type 'function
  :group 'dired-sync)

(defun dired-rsync--default-rsync-failed ()
  "Report rsync failure to console."
  (message "dired-rsync: failed (see %s for details)"
           (current-buffer)))

(defun dired-rsync--pop-to-rsync-failed-buf ()
  "Jump to a recently failed buffer."
  (pop-to-buffer-same-window (current-buffer)))

(defcustom dired-rsync-failed-hook '(dired-rsync--default-rsync-failed)
  "Hook run when rsync fails.
It is run in the context of the failed process buffer."
  :type 'hook
  :group 'dired-rsync)

;; Internal variables
(defvar dired-rsync-proc-buffer-prefix "*rsync"
  "Prefix for process buffers.")

(defvar dired-rsync-modeline-status
  ""
  "A string defining current `dired-rsync' status, useful for modelines.")

(defvar dired-rsync-passphrase-stall-regex
  (rx "Enter passphrase for key")
  "A regex to detect passphrase prompts.")

(defvar dired-rsync-percent-complete-regex
  (rx (** 1 3 digit) "%")
  "A regex to extract the % complete from a file.")

(defvar dired-remote-portfwd
  "ssh -p %d -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null"
  "An explicit ssh command for rsync to use port forwarded proxy.
The string is treated as a format string where %d is replaced with the
results of `dired-rsync--get-remote-port'.")

;; Helpers
(defun dired-rsync--get-remote-port ()
  "Return the remote port we shall use for the reverse port-forward."
  (+ 50000 (length (dired-rsync--get-active-buffers))))

(defun dired-rsync--get-remote-portfwd ()
  (format dired-remote-portfwd (dired-rsync--get-remote-port)))

(defun dired-rsync--quote-and-maybe-convert-from-tramp (file-or-path)
  "Reformat a tramp FILE-OR-PATH to one usable for rsync."
  (if (tramp-tramp-file-p file-or-path)
      (with-parsed-tramp-file-name file-or-path tfop
        (format "%s%s:\"%s\"" (if tfop-user (format "%s@" tfop-user) "") tfop-host
                (shell-quote-argument tfop-localname)))
    (shell-quote-argument file-or-path)))

(defun dired-rsync--extract-host-from-tramp (file-or-path &optional split-user)
  "Extract the tramp host part of FILE-OR-PATH.

It SPLIT-USER is set we remove the user@ part as well.  We assume
hosts don't need quoting."
  (with-parsed-tramp-file-name file-or-path tfop
    (if (or split-user (not tfop-user))
        tfop-host
      (format "%s@%s" tfop-user tfop-host))))

;; This is tricky for remote-to-remote because we may have an implied
;; user from the local config which isn't available on the remote
;; .ssh/config
(defun dired-rsync--extract-user-from-tramp (file-or-path)
  "Extract the username part of a tramp FILE-OR-PATH."
  (with-parsed-tramp-file-name file-or-path tfop
    (or tfop-user
        ; somehow extract .ssh/config user for tfop-host
        (getenv "USER"))))


(defun dired-rsync--extract-paths-from-tramp (files)
  "Extract the path part of a tramp FILES and quote it."
  (--map
   (with-parsed-tramp-file-name it tfop
     (shell-quote-argument tfop-localname))
   files))


;; Count active buffers
(defun dired-rsync--get-proc-buffers ()
  "Return all dired-rsync process buffers."
  (--filter
   (and (s-starts-with? dired-rsync-proc-buffer-prefix (buffer-name it))
        (get-buffer-process it))
   (buffer-list)))

(defun dired-rsync--get-active-buffers ()
  "Return all currently active process buffers"
  (--filter
   (process-live-p (get-buffer-process it))
   (dired-rsync--get-proc-buffers)))


;; Update status with count/speed
(defun dired-rsync--update-modeline (&optional err ind)
  "Update the modeline, optionally with `ERR' or `IND'.

`ERR' is set this indicates a problem, otherwise `IND' is an
alternative indication (such as a percentage completion).  If
neither is set we simply display the current number of jobs."
  (force-mode-line-update)
  (let ((job-count (length (dired-rsync--get-active-buffers))))
    (setq dired-rsync-modeline-status
          (cond
           ;; error has occurred
           (err (propertize
                 (format " R:%d %s!!" job-count err)
                 'font-lock-face '(:foreground "red")))
           ;; we still have jobs but no error
           ((> job-count 1)
            (format " R:%d" job-count))
           ((> job-count 0)
            (format " R:%s" (or ind job-count)))
           ;; Any stale?
           ((dired-rsync--get-proc-buffers)
            (propertize
                 " R:hung :-("
                 'font-lock-face '(:foreground "red")))
           ;; nothing going on
           (t nil)))))

;;
;; Running rsync: We need to take care of a couple of things here. We
;; need to ensure we run from the local host as you shouldn't expect
;; the remote target to be as aware of the ssh shortcuts home as from
;; the local system out (.ssh/config). We also want to track when it
;; is finished so we can inform the user the copy is complete.
;;

(defun dired-rsync--sentinel(proc desc details)
  "Process sentinel for rsync processes.
This gets called whenever the inferior `PROC' changes state as
  described by `DESC'.  `DETAILS' provides access to additional
  information such as the locate of the dired-buffer."
  (let ((proc-buf (process-buffer proc)))
    (when (s-starts-with-p "finished" desc)
      ;; clean-up finished tasks
      (let ((dired-buf (plist-get details ':dired-buffer)))
        (when (and dired-rsync-unmark-on-completion
                   (buffer-live-p dired-buf))
          (with-current-buffer dired-buf
            (dired-unmark-all-marks)))
        (kill-buffer proc-buf)))
    (dired-rsync--update-modeline)
    ;; If we still have a process buffer things didn't end well
    (when (and (not (process-live-p proc))
               (buffer-name proc-buf))
      (with-current-buffer proc-buf
        (run-hooks 'dired-rsync-failed-hook)))))


(defun dired-rsync--filter (proc string details)
  "`PROC' rsync process filter, insert `STRING' into buffer with `DETAILS'.

This gets called with string whenever there is new data to
display in the process buffer.  We scan the string to extract useful
information and can use `DETAILS' to find and update the original
dired-buffer modeline."

  ;; scan the new string
  (let ((err nil) (indicator nil))
    ;; Grab % complete string
    (when (string-match dired-rsync-percent-complete-regex string)
      (setq indicator (match-string 0 string)))
    ;; check for prompt
    (when (string-match dired-rsync-passphrase-stall-regex string)
      (process-send-string proc (concat (read-passwd string) "\n")))
    ;; update if anything to report
    (when (or err indicator)
      (dired-rsync--update-modeline err indicator)))

  ;; update the process buffer (we could just drop?)
  (let ((old-process-mark (process-mark proc)))
    ;; do the normal buffer text insertion
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (let ((moving (= (point) old-process-mark)))
          (save-excursion
            ;; Insert the text, advancing the process marker.
            (goto-char old-process-mark)
            (insert string)
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc))))))))


(defun dired-rsync--do-run (command details)
  "Run rsync COMMAND in a unique buffer, passing DETAILS to sentinel."
  (let* ((buf (format "%s @ %s" dired-rsync-proc-buffer-prefix (current-time-string)))
         (proc (start-process-shell-command "*rsync*" buf command)))
    (set-process-sentinel
     proc
     #'(lambda (proc desc)
         (dired-rsync--sentinel proc desc details)))
    (set-process-filter
     proc
     #'(lambda (proc string)
         (dired-rsync--filter proc string details)))
    (dired-rsync--update-modeline)))

(defun dired-rsync--remote-to-from-local-cmd (sfiles dest)
  "Construct a rsync command for SFILES to DEST copy.

This handles both remote to local or local to remote copy.
Fortunately both forms are broadly the same."
  (let ((src-files
         (-map 'dired-rsync--quote-and-maybe-convert-from-tramp sfiles))
        (final-dest (dired-rsync--quote-and-maybe-convert-from-tramp dest)))
    (s-join " "
            (-flatten
             (list dired-rsync-command
                   dired-rsync-options
                   src-files
                   final-dest)))))

;; ref: https://unix.stackexchange.com/questions/183504/how-to-rsync-files-between-two-remotes
(defun dired-rsync--remote-to-remote-cmd (shost sfiles duser dhost dpath)
  "Construct and trigger an rsync run for remote copy.
The source SHOST and SFILES to remote DUSER @ DHOST to DPATH.

rsync doesn't support this mode of operation but we can fake it by
providing a port forward from the source host which we pass onto the
destination.  This requires ssh'ing to the source and running the rsync
there."
  (s-join " " (-flatten
               (list "ssh" "-A"
                     "-R" (format "localhost:%d:%s:22"
                                  (dired-rsync--get-remote-port) dhost)
                     shost
                     (format
                      "\"%s %s -e \\\"%s\\\" %s %s@localhost:%s\""
                      dired-rsync-command
                      dired-rsync-options
                      (dired-rsync--get-remote-portfwd)
                      (s-join " " sfiles)
                      duser
                      dpath)))))


;;;###autoload
(defun dired-rsync (dest)
  "Asynchronously copy files in dired to `DEST' using rsync.

`DEST' can be a relative filename and will be processed by
`expand-file-name' before being passed to the rsync command.

This function runs the copy asynchronously so Emacs won't block whilst
the copy is running.  It also handles both source and destinations on
ssh/scp tramp connections."
  ;; Interactively grab dest if not called with
  (interactive
   (list (read-file-name "rsync to: " (dired-dwim-target-directory)
                         nil nil nil 'file-directory-p)))

  (setq dest (expand-file-name dest))

  (let ((sfiles (funcall dired-rsync-source-files))
        (cmd))
    (setq cmd
          (if (and (tramp-tramp-file-p dest)
                   (tramp-tramp-file-p (-first-item sfiles)))
              (let ((shost (dired-rsync--extract-host-from-tramp (-first-item sfiles)))
                    (src-files (dired-rsync--extract-paths-from-tramp sfiles))
                    (dhost (dired-rsync--extract-host-from-tramp dest t))
                    (duser (dired-rsync--extract-user-from-tramp dest))
                    (dpath (-first-item (dired-rsync--extract-paths-from-tramp (list dest)))))
                (dired-rsync--remote-to-remote-cmd shost src-files
                                                   duser dhost dpath))
            (dired-rsync--remote-to-from-local-cmd sfiles dest)))
    (dired-rsync--do-run cmd
                         (list :marked-files sfiles
                               :dired-buffer (current-buffer)))))

(provide 'dired-rsync)
;;; dired-rsync.el ends here
