;;; lang-lisp.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Slime
;; =============================================================================
(straight-use-package 'slime)

(add-to-list 'auto-mode-alist '("\\.cl$|\\.lisp$" . slime-mode))

(customize-set-variable 'inferior-lisp-program "/usr/bin/sbcl")
(customize-set-variable 'slime-net-coding-system 'utf-8-unix)
(customize-set-variable 'slime1-contribs '(slime-fancy))
;; =============================================================================

(provide 'lang-lisp)

;;; lang-lisp.el ends here
