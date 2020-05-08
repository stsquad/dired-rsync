;;; dired-rsync-ert --- tests for dired-rsync
;;
;;; Commentary:
;;
;; Mostly this is for the internal functions as it is hard to test
;; remote SSH accessible resources in a test case.
;;
;;; Code:

(require 'ert)

(when (require 'undercover nil t)
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
  (should (string-equal "username@192.168.1.1:\"/blat/blot/\""
                        (dired-rsync--quote-and-maybe-convert-from-tramp "/scp:username@192.168.1.1:/blat/blot/")))
  (should (string-equal "192.168.1.1:\"/blat/blot/\""
                        (dired-rsync--quote-and-maybe-convert-from-tramp "/scp:192.168.1.1:/blat/blot/"))))

(ert-deftest dired-rsync-test-remote-port()
  "Test the remote port handling."
  (should (= 50000 (dired-rsync--get-remote-port)))
  (cl-letf (((symbol-function 'dired-rsync--get-active-buffers) (lambda() '(1 2))))
   (should (= 50002 (dired-rsync--get-remote-port)))))

(ert-deftest dired-rsync-test-remote-remote-cmd ()
  "Test we generate a good remote to remote command."
  (should (string-equal
           "ssh -A -R localhost:50000:host:22 seed \"rsync -az --info=progress2 -e \\\"ssh -p 50000 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null\\\" a b c's user@localhost:/video\""
           (dired-rsync--remote-to-remote-cmd "seed" '("a" "b" "c's") "user"
                                              "host" "/video")))
  (cl-letf (((symbol-function 'dired-rsync--get-active-buffers) (lambda() '(1 2))))
    (should (string-equal
           "ssh -A -R localhost:50002:host:22 seed \"rsync -az --info=progress2 -e \\\"ssh -p 50002 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null\\\" a b c's user@localhost:/video\""
           (dired-rsync--remote-to-remote-cmd "seed" '("a" "b" "c's") "user"
                                              "host" "/video")))))

;;; dired-rsync-ert.el ends here
