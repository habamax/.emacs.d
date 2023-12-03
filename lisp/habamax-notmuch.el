;;; habamax-notmuch.el -- notmuch settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq notmuch-show-logo nil
      notmuch-hello-auto-refresh t
      notmuch-hello-recent-searches-max 20
      notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                               notmuch-hello-insert-alltags)
      notmuch-show-all-tags-list t)
(setq notmuch-search-oldest-first nil
      notmuch-show-empty-saved-searches t
      notmuch-saved-searches
      `((:name "Unread"
               :query "folder:/Inbox/ and tag:unread"
               :sort-order newest-first
               :key ,(kbd "u"))
        (:name "All"
               :query "folder:/Inbox/ or tag:inbox"
               :sort-order newest-first
               :key ,(kbd "a"))))

(defun notmuch-sync ()
  (interactive)
  (when (executable-find "mbsync")
    (notmuch-delete)
    (compile "mbsync -a && notmuch new")))

(defun notmuch-delete ()
  "Delete emails marked with `deleted' tag."
  (interactive)
  (let* ((del-tag "deleted")
         (count
          (string-to-number
           (with-temp-buffer
             (shell-command
              (format "notmuch count tag:%s" del-tag) t)
             (buffer-substring-no-properties (point-min) (1- (point-max))))))
         (mail (if (> count 1) "mails" "mail")))
    (when (> count 0)
      (message "Deleting %d %s marked as `%s'..." count mail del-tag)
      (shell-command
       (format "notmuch search --output=files --format=text0 tag:%s | %s"
               del-tag "xargs -r0 rm")))))

(provide 'habamax-notmuch)
;;; habamax-notmuch.el ends here
