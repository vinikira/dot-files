;;; lang-rust.el --- Rust Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Rustic
;; =============================================================================
(straight-use-package 'rustic)

(customize-set-variable 'rustic-lsp-client 'eglot)
;; =============================================================================

;; LSP
;; =============================================================================
(defvar-local rust-analyzer-link
  (cond
   ((eq system-type 'darwin)
    "https://github.com/rust-analyzer/rust-analyzer/releases/download/2022-02-28/rust-analyzer-x86_64-apple-darwin.gz")
   (t "https://github.com/rust-analyzer/rust-analyzer/releases/download/2022-02-28/rust-analyzer-x86_64-unknown-linux-musl.gz")))

(defvar-local rust-analyzer-command
  (cond
   ((eq system-type 'darwin)
    "rust-analyzer-x86_64-apple-darwin")
   (t "rust-analyzer-x86_64-unknown-linux-musl")))

(declare-function vs/add-auto-lsp-server "layer-lsp.el")

(vs/add-auto-lsp-server 'rustic-mode rust-analyzer-link rust-analyzer-command)
;; =============================================================================

(provide 'lang-rust)

;;; lang-rust.el ends here
