;;; lang-javascript.el --- JavaScript Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; JS Mode
;; =============================================================================
(declare-function js-jsx-enable "js")

(customize-set-variable 'js-indent-level 2)

(add-hook 'javascript-mode-hook #'js-jsx-enable)
;; =============================================================================

;; TypeScript Mode
;; =============================================================================
(straight-use-package 'typescript-mode)
;; =============================================================================

;; Mocha
;; =============================================================================
(straight-use-package 'mocha)

(customize-set-variable 'mocha-reporter "spec")

(with-eval-after-load 'js
  (add-to-list 'auto-mode-alist '("\\.mjs$" . javascript-mode))

  (declare-function mocha-test-project "ext:mocha")
  (when (boundp 'js-mode-map)
    (define-key js-mode-map (kbd "C-c , v") #'mocha-test-project)))
;; =============================================================================

;; ESLint compilation mode
;; =============================================================================
(straight-use-package
 '(eslint :type git :host github :repo "Fuco1/compile-eslint" :branch "master"))

(with-eval-after-load 'js
  (require 'compile-eslint)
  (when (boundp 'compilation-error-regexp-alist)
    (push 'eslint compilation-error-regexp-alist)))
;; =============================================================================

;; LSP
;; =============================================================================
(declare-function vs/add-auto-lsp-server "layer-lsp.el")

(cl-loop for mode in '(typescript-mode js-mode)
         do
         (vs/add-auto-lsp-server
          mode
          :download-fn (lambda (_reinstall)
                         (async-shell-command
                          "npm install -g typescript-language-server typescript"
                          (get-buffer-create "*Install js/ts LSP*")))))
;; =============================================================================

(provide 'lang-javascript)

;;; lang-javascript.el ends here
