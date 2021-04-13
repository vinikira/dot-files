;;; lang-javascript.el --- JavaScript Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; JS Mode
;; =============================================================================
(add-hook 'javascript-mode-hook #'js-jsx-enable)
;; =============================================================================

;; TypeScript Mode
;; =============================================================================
(straight-use-package 'typescript-mode)
;; =============================================================================

;; Mocha
;; =============================================================================
(straight-use-package 'mocha)

(customize-set-variable 'mocha-reporter "spec")

(with-eval-after-load 'js-mode
   (define-key js-mode-map (kbd "C-c , v") 'mocha-test-project))
;; =============================================================================

(provide 'lang-javascript)

;;; lang-javascript.el ends here
