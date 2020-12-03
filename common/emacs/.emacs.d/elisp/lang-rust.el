;;; lang-rust.el --- Rust Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Rust Mode
;; =============================================================================
(straight-use-package 'rust-mode)
(customize-set-variable 'rust-format-on-save t)
(customize-set-variable 'company-tooltip-align-annotations t)
;; =============================================================================

;; Flycheck Rust
;; =============================================================================
(straight-use-package 'flycheck-rust)

(add-hook 'rust-mode-hook 'flycheck-rust-setup)
;; =============================================================================

;; Cargo
;; =============================================================================
(straight-use-package 'cargo)

(add-hook 'rust-mode-hook 'cargo-minor-mode)
;; =============================================================================

(provide 'lang-rust)

;;; lang-rust.el ends here
