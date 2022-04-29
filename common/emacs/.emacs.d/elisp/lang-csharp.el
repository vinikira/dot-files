;;; lang-csharp.el --- CSharp layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; CSharp mode
;; =============================================================================
(straight-use-package 'csharp-mode)
;; =============================================================================

;; LSP
;; =============================================================================
(defcustom vs/omnisharp-solution-file nil
  "Set the solution file for omnisharp LSP server."
  :group 'csharp
  :type 'string
  :safe t)

(defvar-local omnisharp-link
  (concat "https://github.com/omnisharp/omnisharp-roslyn/releases/latest/download/"
          (cond
           ((eq system-type 'darwin) "omnisharp-osx.tar.gz")
           (t "omnisharp-linux-x64.tar.gz"))))

(defvar-local omnisharp-command (append
                                 (list "run" "-lsp")
                                 (when vs/omnisharp-solution-file
                                   (list "-s" vs/omnisharp-solution-file))))

(declare-function vs/add-auto-lsp-server "layer-lsp.el")
(declare-function vs/--wrap-lsp-context "layer-lsp.el")

(vs/add-auto-lsp-server 'csharp-mode
                        :download-url omnisharp-link
                        :command-fn (lambda (_interactive)
                                      (append
                                       (vs/--wrap-lsp-context 'csharp-mode "run")
                                       (list "-lsp")
                                       (when vs/omnisharp-solution-file
                                         (list
                                          "-s"
                                          (expand-file-name vs/omnisharp-solution-file))))))
;; =============================================================================

(provide 'lang-csharp)

;;; lang-csharp.el ends here
