;;; lang-erlang.el --- Erlang Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Erlang Mode
;; ========================================================================================
(straight-use-package 'erlang)

(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
;; ========================================================================================

(provide 'lang-erlang)

;;; lang-erlang.el ends here
