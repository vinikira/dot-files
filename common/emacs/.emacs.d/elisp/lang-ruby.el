;;; lang-ruby.el --- Ruby Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Flymake Ruby
;; =============================================================================
(straight-use-package 'flymake-ruby)

(add-hook 'ruby-mode-hook 'flymake-ruby-load)
;; =============================================================================

(provide 'lang-ruby)

;;; lang-ruby.el ends here
