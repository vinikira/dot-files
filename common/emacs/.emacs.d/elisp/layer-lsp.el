;;; layer-lsp.el --- LSP Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Eglot
;; =============================================================================
(straight-use-package 'eglot)

(with-eval-after-load 'eglot
  (when (boundp 'eglot-mode-map)
    (define-key eglot-mode-map (kbd "M-RET") 'eglot-code-actions)
    (define-key eglot-mode-map (kbd "C-c C-f") 'eglot-format)
    (define-key eglot-mode-map (kbd "C-c C-r") 'eglot-rename)
    (define-key eglot-mode-map (kbd "C-c C-o") 'eglot-code-action-organize-imports)
    (define-key eglot-mode-map (kbd "C-c C-d") 'eldoc)))

(customize-set-variable 'eglot-autoshutdown t)
;; =============================================================================

;; Automatic download LSP servers
;; =============================================================================
(defvar vs/--lsp-servers '()
  "List of LSP servers to download.")

(defvar vs/--lsp-install-dir
  (expand-file-name
   (concat user-emacs-directory "cache/lsp/"))
  "Path to save LSP servers.")

(defun vs/download-lsp-server (reinstall)
  "Download the lsp server for current major mode.
If REINSTALL is provided, it removes old directory and reinstall server."
  (interactive "P")
  (let ((download-handler
         (alist-get major-mode vs/--lsp-servers)))
    (unless download-handler
      (user-error "Major mode (%s) doesn't support auto download yet"
                  major-mode))
    (pcase download-handler
      (`(:download-url . ,url)
       (vs/--download-lsp-server url reinstall))
      (`(:download-fn . ,fn)
       (funcall fn reinstall))
      (_ (user-error "Unsupported download handler: %s" download-handler)))))

(defun vs/add-auto-lsp-server (mode &rest args)
  "Set a language server settings provided by ARGS for MODE."
  (when-let ((download-url (plist-get args :download-url)))
    (add-to-list
     'vs/--lsp-servers
     `(,mode . (:download-url . ,download-url))))

  (when-let ((download-fn (plist-get args :download-fn)))
    (add-to-list
     'vs/--lsp-servers
     `(,mode . (:download-fn . ,download-fn))))

  (with-eval-after-load 'eglot
    (when-let* ((command (plist-get args :command))
                (server-command (append
                                 (vs/--wrap-lsp-context mode (car command))
                                 (cdr command))))
      (when (boundp 'eglot-server-programs)
        (add-to-list 'eglot-server-programs
                     `(,mode . ,server-command))))

    (when-let ((command (plist-get args :command-fn)))
      (when (boundp 'eglot-server-programs)
        (add-to-list 'eglot-server-programs
                     `(,mode . ,command))))))

(defun vs/--wrap-lsp-context (mode command)
  "Wrap COMMAND for MODE in the LSP context."
  (list (expand-file-name
         (concat vs/--lsp-install-dir
                 (symbol-name mode)
                 "/"
                 command))))

(defun vs/--download-lsp-server (download-url reinstall)
  "Download the LSP server to the cache directory using DOWNLOAD-URL.
When REINSTALL is t deletes the current server directory."
  (declare-function dired-compress-file "ext:dired-aux")
  (let* ((server-directory (concat
                            vs/--lsp-install-dir
                            (symbol-name major-mode)))
         (default-directory server-directory)
         (file-name (car (last (split-string download-url "/"))))
         (file-path (concat server-directory "/" file-name)))
    (when reinstall
      (delete-directory server-directory t))
    (if (not (file-exists-p server-directory))
        (progn
          (make-directory server-directory t)
          (message "Downloading LSP server for %s..." major-mode)
          (url-copy-file download-url file-path)
          (dired-compress-file file-path)
          (chmod file-path #o755))
      (message "Server already installed."))))
;; =============================================================================

(provide 'layer-lsp)

;;; layer-lsp.el ends here
