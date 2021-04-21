;;; lang-lisp.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Slime
;; =============================================================================
(straight-use-package 'slime)

(add-to-list 'auto-mode-alist '("\\.cl$|\\.lisp$" . slime-mode))

(customize-set-variable 'inferior-lisp-program "sbcl")
(customize-set-variable 'slime1-contribs '(slime-fancy))
(customize-set-variable 'slime-net-coding-system 'utf-8-unix)
;; =============================================================================

(provide 'lang-lisp)

;;; lang-lisp.el ends here
