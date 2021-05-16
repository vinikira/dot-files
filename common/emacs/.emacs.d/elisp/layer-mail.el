;;; layer-mail.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Notmuch mail client
;; =============================================================================
(autoload 'notmuch "notmuch" "notmuch mail" t)

(customize-set-variable 'notmuch-saved-searches
                        '((:name "Unread"
                           :query "tag:inbox and tag:unread"
                           :count-query "tag:inbox and tag:unread"
                           :key "u"
                           :sort-order newest-first)
                          (:name "Inbox"
                           :query "tag:inbox"
                           :count-query "tag:inbox"
                           :key "i"
                           :sort-order newest-first)
                          (:name "Archive"
                           :query "tag:archive"
                           :count-query "tag:archive"
                           :key "a"
                           :sort-order newest-first)
                          (:name "Sent"
                           :query "tag:sent or tag:replied"
                           :count-query "tag:sent or tag:replied"
                           :key "s"
                           :sort-order newest-first)
                          (:name "Trash"
                           :query "tag:deleted"
                           :count-query "tag:deleted"
                           :key "t"
                           :sort-order newest-first)))
;; =============================================================================

;; SMTP
;; =============================================================================
;; Remember to set the `user-full-name' and `user-mail-address' in custom file.

(customize-set-variable 'mail-user-agent 'message-user-agent)
(customize-set-variable 'mail-specify-envelope-from t)
(customize-set-variable 'sendmail-program "msmtp")
(customize-set-variable 'mail-specify-envelope-from t)
(customize-set-variable 'mail-envelope-from 'header)
(customize-set-variable 'message-sendmail-envelope-from 'header)
(customize-set-variable 'message-send-mail-function 'message-send-mail-with-sendmail)
(customize-set-variable 'message-kill-buffer-on-exit t)
;; =============================================================================

(provide 'layer-mail)

;;; layer-mail.el ends here
