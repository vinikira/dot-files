;;; lang-erlang.el --- Erlang Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Erlang Mode
;; ========================================================================================
(defun vs/load-erlang-mode ()
  "Detect if erlang is installed and load elisp files from erlang directory."
  (interactive)
  (let* ((erlang-lib-dir (concat
                          (string-trim (shell-command-to-string "asdf where erlang"))
                          "/lib"))
         (tools-dir (seq-find (lambda (dir-name)
                                (string-match "^tools.?+" dir-name))
                              (directory-files erlang-lib-dir)))
         (erlang-emacs-dir (concat erlang-lib-dir "/" tools-dir "/emacs")))
    (if (file-directory-p erlang-emacs-dir)
        (progn
          (add-to-list 'load-path erlang-emacs-dir)
          (require 'erlang)
          (message "Erlang mode loaded!"))
      (warn "Erlang isn't installed..."))))
;; ========================================================================================

(provide 'lang-erlang)

;;; lang-erlang.el ends here
