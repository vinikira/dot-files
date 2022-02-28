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
;; =============================================================================

;; Toggle LSP per project
;; =============================================================================
(defvar vs/lsp-allowed-projects '()
  "Projects that are allowed to use LSP in current session.")

(declare-function eglot-shutdown "ext:eglot")
(declare-function eglot "ext:eglot")
(declare-function project--buffer-list "ext:project")
(declare-function project-root "ext:project")

(defun vs/toggle-lsp-for-current-project ()
  "Enable/disable `eglot' for current project."
  (interactive)
  (let ((disabled nil)
        (current-project (expand-file-name (project-root (project-current)))))
    (when (eq current-project nil)
      (error "The current buffer is not part of any project"))
    (if (member current-project vs/lsp-allowed-projects)
        (progn
          (setq disabled t
                vs/lsp-allowed-projects
                (delete current-project vs/lsp-allowed-projects)))
      (push current-project vs/lsp-allowed-projects))
    (dolist (buffer (project--buffer-list (project-current)))
      (when (buffer-file-name buffer)
        (with-current-buffer buffer
          (if disabled
              (call-interactively #'eglot-shutdown)
            (call-interactively #'eglot)))))))
;; =============================================================================

;; Automatic download lsp servers
;; =============================================================================
(defvar vs/--lsp-servers '()
  "List of LSP servers to download.")

(defvar vs/--lsp-install-dir (expand-file-name
                              (concat user-emacs-directory "cache/lsp/"))
  "Path to save LSP servers.")

(defun vs/download-lsp-server (reinstall)
  "Download the lsp server for current major mode.
If REINSTALL is provided, it removes old directory and reinstall server."
  (interactive "P")
  (let ((download-handler
         (alist-get major-mode vs/--lsp-servers)))
    (unless download-handler
      (error "Major mode (%s) doesn't support auto download yet."
             major-mode))
    (cond
     ((functionp download-handler)
      (funcall download-handler reinstall))
     ((stringp download-handler)
      (vs/--download-lsp-server download-handler reinstall))
     (t (error "Unsupported download handler: %s" download-handler)))))

(defun vs/add-auto-lsp-server (mode download-handler &optional command)
  "Set a language server DOWNLOAD-HANDLER and
optinally a custom COMMAND for execute the server."
  (add-to-list
   'vs/--lsp-servers
   `(,mode . ,download-handler))
  (when (and (boundp 'eglot-server-programs) command)
    (let ((server-program (expand-file-name
                           (concat vs/--lsp-install-dir
                                   (symbol-name mode)
                                   "/"
                                   command))))
      (add-to-list 'eglot-server-programs
                   `(,mode . (,server-program))))))

(defun vs/--download-lsp-server (download-link reinstall)
  "Download the LSP server to the cache directory using DOWNLOAD-LINK.
When REINSTALL is t deletes the current server directory."
  (declare-function dired-compress-file "ext:dired-aux")
  (let* ((server-directory (concat
                            vs/--lsp-install-dir
                            (symbol-name major-mode)))
         (file-name (car (last (split-string download-link "/"))))
         (file-path (concat server-directory "/" file-name)))
    (when reinstall
      (delete-directory server-directory t))
    (if (not (file-exists-p server-directory))
        (progn
          (make-directory server-directory t)
          (message "Downloading LSP server for %s..." major-mode)
          (url-copy-file download-link file-path)
          (pcase (file-name-extension file-name)
            ("zip"
             (dired-compress-file file-path))
            ("tar.gz"
             (dired-compress-file file-path))
            ("tar.xz"
             (dired-compress-file file-path))
            (_ (chmod file-path 755))))
      (message "Server already installed."))))
;; =============================================================================

(provide 'layer-lsp)

;;; layer-lsp.el ends here
