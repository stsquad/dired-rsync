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

(ert-deftest dired-rsync-test-remote-remote-cmd ()
  "Test we generate a good remote to remote command."
  (should (string-equal
           "ssh -A -R localhost:50000:host:22 seed 'rsync -az --info=progress2 -e \"ssh -p 50000 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null\" a b c's user@localhost:/video'"
           (dired-rsync--remote-to-remote-cmd "seed" '("a" "b" "c's") "user"
                                              "host" "/video"))))

(ert-deftest dired-rsync-test-command ()
  (should (string= "rsync -az --info=progress2 remote.host:\"/a.file\" remote.host:\"/b.file\" /home/user"
                   (dired-rsync--command '("/ssh:remote.host:/a.file" "/ssh:remote.host:/b.file") "/home/user"))))




;;; dired-rsync-ert.el ends here
