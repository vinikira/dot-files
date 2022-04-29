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
  (concat "https://github.com/clojure-lsp/clojure-lsp/releases/latest/download/"
          (cond
           ((eq system-type 'darwin) "clojure-lsp-native-macos-amd64.zip")
           (t "clojure-lsp-native-static-linux-amd64.zip"))))

(defvar-local clojure-lsp-command
  (cond
   ((eq system-type 'darwin)
    '("clojure-lsp-native-macos-amd64/clojure-lsp"))
   (t '("clojure-lsp-native-static-linux-amd64/clojure-lsp"))))

(declare-function vs/add-auto-lsp-server "layer-lsp.el")

(vs/add-auto-lsp-server 'clojure-mode
                        :download-url clojure-lsp-link
                        :command clojure-lsp-command)
;; =============================================================================

(provide 'lang-clojure)
;;; lang-clojure.el ends here
