;;; lang-erlang.el --- Erlang Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Erlang Mode
;; ========================================================================================
(defun vs/load-erlang-mode ()
  "Detect if erlang is installed and load elisp files from erlang directory."
  (interactive)
  (let ((erlang-dir (concat
                     (string-trim (shell-command-to-string "asdf where erlang"))
                     "/lib/tools-3.5/emacs")))
    (if (file-directory-p erlang-dir)
        (progn
          (add-to-list 'load-path erlang-dir)
          (require 'erlang)
          (message "Erlang mode loaded!"))
      (warn "Erlang isn't installed..."))))
;; ========================================================================================

(provide 'lang-erlang)

;;; lang-erlang.el ends here
