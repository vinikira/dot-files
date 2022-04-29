;;; lang-dart.el --- Dart Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Dart Mode
;; =============================================================================
(straight-use-package 'dart-mode)

(customize-set-variable 'dart-format-on-save t)
;; =============================================================================

;; LSP
;; =============================================================================
(declare-function vs/add-auto-lsp-server "layer-lsp.el")

(vs/add-auto-lsp-server
 'dart-mode
 :command-fn (lambda () (list "dart" "language-server")))
;; =============================================================================

(provide 'lang-dart)

;;; lang-dart.el ends here
