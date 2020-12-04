;;; lang-dart.el --- Dart Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Dart Mode
;; =============================================================================
(straight-use-package 'dart-mode)

(customize-set-variable 'dart-format-on-save t)

(add-to-list 'auto-mode-alist '("\\.dart$" . dart-mode))
;; =============================================================================

;; LSP Dart
;; =============================================================================
(straight-use-package 'lsp-dart)

(add-hook 'dart-mode-hook 'lsp-dart)

;; =============================================================================

(provide 'lang-dart)

;;; lang-dart.el ends here
