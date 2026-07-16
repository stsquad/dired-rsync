;;; dired-rsync-ert --- tests for dired-rsync
;;
;;; Commentary:
;;
;; Mostly this is for the internal functions as it is hard to test
;; remote SSH accessible resources in a test case.
;;
;;; Code:

(require 'ert)

;;
(when (and (version<= "26.1" emacs-version)
           (require 'undercover nil t))
  (undercover "dired-rsync.el"))

(require 'dired-rsync)

(ert-deftest dired-rsync-test-extract-host ()
  "Test the various extractions of host from sources."
  (should (string-equal "host"
                        (dired-rsync--extract-host-from-tramp
                         "/ssh:host:/path/to/file.txt")))
  (should (string-equal "user@host"
                        (dired-rsync--extract-host-from-tramp
                         "/ssh:user@host:/path/to/file.txt")))
  (should (string-equal "host"
                        (dired-rsync--extract-host-from-tramp
                         "/ssh:user@host:/path/to/file.txt" t))))

(ert-deftest dired-rsync-test-extract-user ()
  "Test the various extractions of user from paths."
  (should (string-equal "user"
                        (dired-rsync--extract-user-from-tramp
                         "/ssh:user@host:/path/to/file.txt")))
  (let ((tramp-default-user "wibble"))
    (should (string-equal "wibble"
                          (dired-rsync--extract-user-from-tramp
                           "/ssh:host:/path/to/file.txt")))))

(ert-deftest dired-rsync-test-extract-port ()
  "Test the various extractions of port from paths."
  (should-not (dired-rsync--extract-port-from-tramp "/path/to/file.txt"))
  (should (string-equal "1022"
                        (dired-rsync--extract-port-from-tramp
                         "/ssh:user@host#1022:/path/to/file.txt"))))

(ert-deftest dired-rsync-test-extract-path()
  "Test the various extractions of the path."
  (should (string-equal "/path/to/file.txt"
                        (car (dired-rsync--extract-paths-from-tramp
                              '("/ssh:host:/path/to/file.txt"
                                "/ssh:host:/path/to/file2.txt")))))
  (should (string-equal "/path/to/file2.txt"
                        (nth 1 (dired-rsync--extract-paths-from-tramp
                                '("/ssh:host:/path/to/file.txt"
                                  "/ssh:host:/path/to/file2.txt")))))
  (should (string-equal "/path/to/file.txt"
                        (car (dired-rsync--extract-paths-from-tramp
                              '("/ssh:host:/path/to/file.txt")))))
  (should (string-equal "/path/to/pluralised\\'s.txt"
                        (car (dired-rsync--extract-paths-from-tramp
                              '("/ssh:host:/path/to/pluralised's.txt")))))
  (should (string-equal "/path/to/file.txt"
                        (car (dired-rsync--extract-paths-from-tramp
                              '("/ssh:servername|sudo:root@servername:/path/to/file.txt"))))))

(ert-deftest dired-rsync-test-quote-and-maybe-convert-from-tramp ()
  "Test quote and maybe convert from tramp defun"
  ;; test against regression of issue #26: missing username in rsync command
  (should (string-equal "username@192.168.1.1:/blat/blot/"
                        (dired-rsync--quote-and-maybe-convert-from-tramp "/scp:username@192.168.1.1:/blat/blot/")))
  (should (string-equal "192.168.1.1:/blat/blot/"
                        (dired-rsync--quote-and-maybe-convert-from-tramp "/scp:192.168.1.1:/blat/blot/"))))

(ert-deftest dired-rsync-test-remote-port()
  "Test the remote port handling."
  (should (= 50000 (dired-rsync--get-remote-port)))
  (cl-letf (((symbol-function 'dired-rsync--get-active-buffers) (lambda() '(1 2))))
    (should (= 50002 (dired-rsync--get-remote-port)))))

(ert-deftest dired-rsync-test-remote-remote-cmd ()
  "Test we generate a good remote to remote command."
  (should (string-equal
           "ssh -A -R localhost:50000:host:22 seed \"rsync -az --info=progress2 -e \\\"ssh -p 50000 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null\\\" -- a b c's user@localhost:/video\""
           (dired-rsync--remote-to-remote-cmd "seed" nil '("a" "b" "c's") "user"
                                              "host" nil "/video")))
  (should (string-equal
           "ssh -A -p 23 -R localhost:50000:host:1022 seed \"rsync -az --info=progress2 -e \\\"ssh -p 50000 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null\\\" -- a b c's user@localhost:/video\""
           (dired-rsync--remote-to-remote-cmd "seed" "23" '("a" "b" "c's") "user"
                                              "host" "1022" "/video")))
  (cl-letf (((symbol-function 'dired-rsync--get-active-buffers) (lambda() '(1 2))))
    (should (string-equal
             "ssh -A -R localhost:50002:host:22 seed \"rsync -az --info=progress2 -e \\\"ssh -p 50002 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null\\\" -- a b c's user@localhost:/video\""
             (dired-rsync--remote-to-remote-cmd "seed" nil '("a" "b" "c's") "user"
                                                "host" nil "/video")))))

(ert-deftest dired-rsync-test-get-proc-buffers ()
  "Test dired-rsync--get-proc-buffers by mocking buffer-list and get-buffer-process."
  (cl-letf (((symbol-function 'buffer-list) (lambda () '("*rsync @ 1" "*some-other-buffer*" "*rsync @ 2" "*rsync @ 3")))
            ((symbol-function 'buffer-name) (lambda (buf) buf))
            (mock-proc-1 (list :name "proc1" :buffer "*rsync @ 1"))
            (mock-proc-2 (list :name "proc2" :buffer "*rsync @ 2"))
            ((symbol-function 'get-buffer-process) (lambda (buf)
                                                    (cond
                                                     ((string-equal buf "*rsync @ 1") mock-proc-1)
                                                     ((string-equal buf "*rsync @ 2") mock-proc-2)
                                                     (t nil)))))
    (should (equal '("*rsync @ 1" "*rsync @ 2") (dired-rsync--get-proc-buffers)))))

(ert-deftest dired-rsync-test-get-active-buffers ()
  "Test dired-rsync--get-active-buffers by mocking buffer-list, get-buffer-process, and process-live-p."
  (cl-letf (((symbol-function 'buffer-list) (lambda () '("*rsync @ 1" "*some-other-buffer*" "*rsync @ 2" "*rsync @ 3")))
            ((symbol-function 'buffer-name) (lambda (buf) buf))
            (mock-proc-1 (list :name "proc1" :buffer "*rsync @ 1"))
            (mock-proc-2 (list :name "proc2" :buffer "*rsync @ 2"))
            ((symbol-function 'get-buffer-process) (lambda (buf)
                                                    (cond
                                                     ((string-equal buf "*rsync @ 1") mock-proc-1)
                                                     ((string-equal buf "*rsync @ 2") mock-proc-2)
                                                     (t nil))))
            ((symbol-function 'process-live-p) (lambda (proc)
                                                  (cond
                                                   ((equal proc mock-proc-1) t)
                                                   ((equal proc mock-proc-2) nil)
                                                   (t nil)))))
    (should (equal '("*rsync @ 1") (dired-rsync--get-active-buffers)))))

(ert-deftest dired-rsync-test-update-modeline-no-jobs ()
  "Test modeline update with no active jobs and no stale buffers."
  (cl-letf (((symbol-function 'dired-rsync--get-active-buffers) (lambda () nil))
            ((symbol-function 'dired-rsync--get-proc-buffers) (lambda () nil))
            (dired-rsync-modeline-status nil)) ; Reset for each test
    (dired-rsync--update-modeline)
    (should (string-equal "" dired-rsync-modeline-status))))

(ert-deftest dired-rsync-test-update-modeline-one-job ()
  "Test modeline update with one active job and no indicator."
  (cl-letf (((symbol-function 'dired-rsync--get-active-buffers) (lambda () '(t)))
            ((symbol-function 'dired-rsync--get-proc-buffers) (lambda () nil))
            (dired-rsync-modeline-status nil))
    (dired-rsync--update-modeline)
    (should (string-equal " R:1" dired-rsync-modeline-status))))

(ert-deftest dired-rsync-test-update-modeline-one-job-with-indicator ()
  "Test modeline update with one active job and a percentage indicator."
  (cl-letf (((symbol-function 'dired-rsync--get-active-buffers) (lambda () '(t)))
            ((symbol-function 'dired-rsync--get-proc-buffers) (lambda () nil))
            (dired-rsync-modeline-status nil))
    (dired-rsync--update-modeline nil "50%")
    (should (string-equal " R:50%%" dired-rsync-modeline-status))))

(ert-deftest dired-rsync-test-update-modeline-multiple-jobs ()
  "Test modeline update with multiple active jobs."
  (cl-letf (((symbol-function 'dired-rsync--get-active-buffers) (lambda () '(t t)))
            ((symbol-function 'dired-rsync--get-proc-buffers) (lambda () nil))
            (dired-rsync-modeline-status nil))
    (dired-rsync--update-modeline)
    (should (string-equal " R:2" dired-rsync-modeline-status))))

(ert-deftest dired-rsync-test-update-modeline-error ()
  "Test modeline update when an error is present."
  (cl-letf (((symbol-function 'dired-rsync--get-active-buffers) (lambda () '(t)))
            ((symbol-function 'dired-rsync--get-proc-buffers) (lambda () nil))
            (dired-rsync-modeline-status nil))
    (dired-rsync--update-modeline "ErrorMsg")
    (should (string-equal (propertize " R:1 ErrorMsg!!" 'font-lock-face '(:foreground "red"))
                          dired-rsync-modeline-status))))

(ert-deftest dired-rsync-test-update-modeline-stale-buffers ()
  "Test modeline update with stale buffers but no active jobs."
  (cl-letf (((symbol-function 'buffer-list) (lambda () '("*rsync @ 1" "*rsync @ 2")))
            ((symbol-function 'buffer-name) (lambda (buf) buf))
            (mock-proc-1 (list :name "proc1" :buffer "*rsync @ 1"))
            (mock-proc-2 (list :name "proc2" :buffer "*rsync @ 2"))
            ((symbol-function 'get-buffer-process) (lambda (buf)
                                                    (cond
                                                     ((string-equal buf "*rsync @ 1") mock-proc-1)
                                                     ((string-equal buf "*rsync @ 2") mock-proc-2)
                                                     (t nil))))
            ((symbol-function 'process-live-p) (lambda (proc) nil))
            (dired-rsync-modeline-status nil))
    (dired-rsync--update-modeline)
    (should (string-equal (propertize " R:hung :-(" 'font-lock-face '(:foreground "red"))
                          dired-rsync-modeline-status))))

;;; dired-rsync-ert.el ends here
