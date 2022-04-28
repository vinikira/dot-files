;;; lang-ruby.el --- Ruby Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Flymake Ruby
;; =============================================================================
(straight-use-package 'flymake-ruby)

(declare-function flymake-ruby-load "ext:flymake-ruby")

(add-hook 'ruby-mode-hook #'flymake-ruby-load)
;; =============================================================================

;; RSpec mode
;; =============================================================================
(straight-use-package 'rspec-mode)

(declare-function rspec-install-snippets "ext:rspec-mode")

(eval-after-load 'rspec-mode
 '(rspec-install-snippets))
;; =============================================================================

;; inf-ruby
;; =============================================================================
(straight-use-package 'inf-ruby)

(declare-function inf-ruby-minor-mode "ext:inf-ruby")

(add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
;; =============================================================================


(provide 'lang-ruby)

;;; lang-ruby.el ends here
