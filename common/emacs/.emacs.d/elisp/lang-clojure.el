;;; lang-clojure.el --- Clojure Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Clojure mode
;; =============================================================================
(straight-use-package 'clojure-mode)
;; =============================================================================

;; Cider
;; =============================================================================
(straight-use-package 'cider)
;; =============================================================================

;; Flymake Kondor
;; =============================================================================
(straight-use-package 'flymake-kondor)

(declare-function flymake-kondor-setup "ext:flymake-kondor")

(add-hook 'clojure-mode-hook #'flymake-kondor-setup)
;; =============================================================================

;; LSP
;; =============================================================================
(defvar-local clojure-lsp-link
  (cond
   ((eq system-type 'darwin)
    "https://github.com/clojure-lsp/clojure-lsp/releases/download/2022.02.23-12.12.12/clojure-lsp-native-macos-amd64.zip")
   (t "https://github.com/clojure-lsp/clojure-lsp/releases/download/2022.02.23-12.12.12/clojure-lsp-native-static-linux-amd64.zip")))

(defvar-local clojure-lsp-command
  (cond
   ((eq system-type 'darwin)
    "clojure-lsp-native-macos-amd64/clojure-lsp")
   (t "clojure-lsp-native-static-linux-amd64/clojure-lsp")))

(declare-function vs/add-auto-lsp-server "layer-lsp.el")

(vs/add-auto-lsp-server 'clojure-mode clojure-lsp-link clojure-lsp-command)
;; =============================================================================

(provide 'lang-clojure)
;;; lang-clojure.el ends here
