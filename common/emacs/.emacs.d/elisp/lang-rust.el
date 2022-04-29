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
  (concat "https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/"
          (cond
           ((eq system-type 'darwin) "rust-analyzer-x86_64-apple-darwin.gz")
           (t "rust-analyzer-x86_64-unknown-linux-musl.gz"))))

(defvar-local rust-analyzer-command
  (list (cond
         ((eq system-type 'darwin)
          "rust-analyzer-x86_64-apple-darwin")
         (t "rust-analyzer-x86_64-unknown-linux-musl"))))

(declare-function vs/add-auto-lsp-server "layer-lsp.el")

(vs/add-auto-lsp-server 'rustic-mode
                        :download-url rust-analyzer-link
                        :command rust-analyzer-command)
;; =============================================================================

(provide 'lang-rust)

;;; lang-rust.el ends here
