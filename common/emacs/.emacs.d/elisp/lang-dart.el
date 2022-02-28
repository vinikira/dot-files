;;; lang-dart.el --- Dart Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Dart Mode
;; =============================================================================
(straight-use-package 'dart-mode)

(customize-set-variable 'dart-format-on-save t)
;; =============================================================================

(provide 'lang-dart)

;;; lang-dart.el ends here
