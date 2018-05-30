;;; dired-rsync.el --- Allow rsync from dired buffers -*- lexical-binding: t -*-
;;
;; Copyright (C) 2018 Alex Bennée
;;
;; Author: Alex Bennée <alex@bennee.com>
;; Maintainer: Alex Bennée <alex@bennee.com>
;; Version: 0.3
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

(require 'dired-aux) ; for dired-dwim-target-directory
(require 'dash)
(require 's)

;;; Code:

;; Customisation options

(defcustom dired-rsync-command "rsync"
  "The rsync binary that we are going to use."
  :type 'string
  :group 'dired-rsync)

(defcustom dired-rsync-options "-avz --progress"
  "The default options for the rsync command."
  :type 'string
  :group 'dired-rsync)

(defcustom dired-rsync-unmark-on-completion t
  "Control if dired-rsync should unmark when complete."
  :type 'boolean
  :group 'dired-rsync)

;; Internal variables
(defvar dired-rsync-job-count 0
  "Count of running rsync jobs.")

(defvar dired-rsync-modeline-status
  ""
  "A string defining current `dired-rsync' status, useful for modelines.")

;; Helpers

(defun dired-rsync--quote-and-maybe-convert-from-tramp (file-or-path)
  "Reformat a tramp FILE-OR-PATH to one usable for rsync."
  (if (tramp-tramp-file-p file-or-path)
      ;; tramp format is /method:remote:path
      (let ((parts (s-split ":" file-or-path)))
        (format "%s:\"%s\"" (nth 1 parts) (shell-quote-argument (nth 2 parts))))
    (shell-quote-argument file-or-path)))

;; Update status with count/speed
(defun dired-rsync--update-modeline ()
  "Update the number of current jobs."
  (setq mode-line-process
          (setq dired-rsync-modeline-status
                (if (> dired-rsync-job-count 0)
                    (format " R:%d " dired-rsync-job-count)
                  nil))))

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
  described by `DESC'."
  (when (s-starts-with-p "finished" desc)
    ;; clean-up finished tasks
    (let ((proc-buf (process-buffer proc))
          (dired-buf (plist-get details ':dired-buffer)))
      (when dired-rsync-unmark-on-completion
        (with-current-buffer dired-buf
          (dired-unmark-all-marks)))
      (kill-buffer proc-buf)))
  ;; clean-up data left from dead/finished processes
  (when (not (process-live-p proc))
    (setq dired-rsync-job-count (1- dired-rsync-job-count)))
  (dired-rsync--update-modeline))

(defun dired-rsync--do-run (command details)
  "Run rsync COMMAND in a unique buffer, passing DETAILS to sentinel."
  (let* ((buf (format "*rsync @ %s" (current-time-string)))
         (proc (start-process-shell-command "*rsync*" buf command)))
    (lexical-let ((job-details details))
      (set-process-sentinel
       proc
       #'(lambda (proc desc)
           (dired-rsync--sentinel proc desc job-details))))
    (setq dired-rsync-job-count (1+ dired-rsync-job-count))
    (dired-rsync--update-modeline)))

;;;###autoload
(defun dired-rsync (dest)
  "Asynchronously copy files in dired to DEST using rsync.

This function runs the copy asynchronously so Emacs won't block whilst
the copy is running. It also handles both source and destinations on
ssh/scp tramp connections."
  ;; Interactively grab dest if not called with
  (interactive
   (list (read-file-name "rsync to:" (dired-dwim-target-directory))))

  (let ((src-files (-map
                    'dired-rsync--quote-and-maybe-convert-from-tramp
                    (dired-get-marked-files nil current-prefix-arg)))
        (final-dest (dired-rsync--quote-and-maybe-convert-from-tramp dest)))

    ;; now build the rsync command
    (let ((cmd (s-join " "
                       (-flatten
                        (list dired-rsync-command
                              dired-rsync-options
                              src-files
                              final-dest)))))
      (dired-rsync--do-run cmd
                           (list :marked-files src-files
                                 :dired-buffer (buffer-name))))))

(provide 'dired-rsync)
;;; dired-rsync.el ends here
