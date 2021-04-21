;;; lang-java.el --- Java Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Java Mode
;; =============================================================================
(defun vs/--java-hook ()
  "Configures 'java-mode'."
  (require 'cc-mode)
  (c-set-style "cc-mode")
  (make-local-variable 'tab-width)
  (make-local-variable 'indent-tabs-mode)
  (make-local-variable 'c-basic-offset)
  (customize-set-variable 'tab-width 4)
  (customize-set-variable 'indent-tabs-mode t)
  (customize-set-variable 'c-basic-offset 4))

(add-hook 'java-mode-hook #'vs/--java-hook)
;; =============================================================================

;; LSP Java
;; =============================================================================
(straight-use-package 'lsp-java)

(with-eval-after-load 'lsp-java
  (declare-function dap-java-run-test-class "ext:dap-java")
  (declare-function dap-java-run-test-method "ext:dap-java")
  (declare-function dap-java-debug-test-class "ext:dap-java")
  (declare-function dap-java-debug-test-method "ext:dap-java")

  (when (boundp 'java-mode-map)
    (define-key java-mode-map (kbd "C-c , v") #'dap-java-run-test-class)
    (define-key java-mode-map (kbd "C-c , s") #'dap-java-run-test-method)
    (define-key java-mode-map (kbd "C-c , D") #'dap-java-debug-test-class)
    (define-key java-mode-map (kbd "C-c , d") #'dap-java-debug-test-method)))
;; =============================================================================

(provide 'lang-java)

;;; lang-java.el ends here
