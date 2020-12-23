;;; lang-csharp.el --- CSharp layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; CSharp mode
;; =============================================================================
(straight-use-package 'csharp-mode)

(add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode))
;; =============================================================================

(provide 'lang-csharp)

;;; lang-csharp.el ends here
