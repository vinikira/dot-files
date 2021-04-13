;;; lang-dart.el --- Dart Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Dart Mode
;; =============================================================================
(straight-use-package 'dart-mode)

(customize-set-variable 'dart-format-on-save t)
;; =============================================================================

;; LSP Dart
;; =============================================================================
(straight-use-package 'lsp-dart)
;; =============================================================================

(provide 'lang-dart)

;;; lang-dart.el ends here
