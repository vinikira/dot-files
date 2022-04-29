;;; lang-zig.el --- Zig lang config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Zig mode
;; =============================================================================
(straight-use-package 'zig-mode)
;; =============================================================================

;; LSP
;; =============================================================================
(defvar-local zls-link
  (concat "https://github.com/zigtools/zls/releases/latest/download/"
          (cond
           ((eq system-type 'darwin) "x86_64-macos.tar.xz")
           (t "x86_64-linux.tar.xz"))))

(defvar-local zls-command
  (list (cond
         ((eq system-type 'darwin)
          "x86_64-macos/zls")
         (t "x86_64-linux/zls"))))

(declare-function vs/add-auto-lsp-server "layer-lsp.el")

(vs/add-auto-lsp-server 'zig-mode :download-url zls-link :command zls-command)
;; =============================================================================

(provide 'lang-zig)
;;; lang-zig.el ends here
