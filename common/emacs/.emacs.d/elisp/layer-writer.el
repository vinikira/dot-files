;;; layer-writer.el --- Writer Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; LaTeX
;; =============================================================================
(straight-use-package 'auctex)
(straight-use-package 'auctex-latexmk)

(customize-set-variable 'auctex-latexmk-inherit-TeX-PDF-mode t)

(add-hook 'tex-mode-hook #'flyspell-mode)
(add-hook 'auctex-mode-hook #'auctex-latexmk-setup)
;; =============================================================================

;; Epub mode
;; =============================================================================
(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub$" . nov-mode))
;; =============================================================================

(provide 'layer-writer)

;;; layer-writer.el ends here
