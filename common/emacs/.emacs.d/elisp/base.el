;;; base.el --- Base file with Emacs Customizations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Private dir
;; =============================================================================
(defconst private-dir (expand-file-name "private" user-emacs-directory))
(unless (file-exists-p private-dir)
  (make-directory private-dir :parents))
;; =============================================================================

;; Temp dir
;; =============================================================================
(defconst temp-dir (format "%scache" user-emacs-directory))

(unless (file-exists-p temp-dir)
  (make-directory (concat temp-dir) :parents))
;; =============================================================================

;; Custom file
;; =============================================================================
(setq custom-file (expand-file-name (concat private-dir "/custom.el")))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)
;; =============================================================================

;; UTF-8
;; =============================================================================
(customize-set-variable 'default-process-coding-system
			'(utf-8-unix . utf-8-unix))
(customize-set-variable 'locale-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; =============================================================================

;; Emacs Customizations
;; =============================================================================
(customize-set-variable 'confirm-kill-emacs 'y-or-n-p)
(customize-set-variable 'confirm-nonexistent-file-or-buffer t)
(customize-set-variable 'save-interprogram-paste-before-kill t)
(customize-set-variable 'mouse-yank-at-point t)
(customize-set-variable 'require-final-newline t)
(customize-set-variable 'visible-bell nil)
(customize-set-variable 'ring-bell-function 'ignore)
;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(customize-set-variable 'minibuffer-prompt-properties
			'(read-only
			  t
			  point-entered
			  minibuffer-avoid-prompt
			  face minibuffer-prompt))
;; Disable non selected window highlight
(customize-set-variable 'cursor-in-non-selected-windows nil)
(customize-set-variable 'highlight-nonselected-windows nil)
;; PATH
(customize-set-variable 'exec-path (append
				    exec-path
				    `("/usr/local/bin/"
				      ,(expand-file-name "~/.local/bin")
				      ,(expand-file-name "~/.asdf/shims"))))
(customize-set-variable 'inhibit-startup-message t)
(customize-set-variable 'fringes-outside-margins t)
(customize-set-variable 'select-enable-clipboard t)
;; Backups enabled, use nil to disable
(customize-set-variable 'history-length 1000)
(customize-set-variable 'backup-inhibited nil)
(customize-set-variable 'make-backup-files t)
(customize-set-variable 'auto-save-default t)
(customize-set-variable 'auto-save-list-file-name (concat temp-dir "/autosave"))
(customize-set-variable 'create-lockfiles nil)
(customize-set-variable 'backup-directory-alist
			`((".*" . ,(concat temp-dir "/backup/"))))
(customize-set-variable 'auto-save-file-name-transforms
			`((".*" ,(concat temp-dir "/backup/") t)))
(customize-set-variable 'bookmark-save-flag t)
(customize-set-variable 'bookmark-default-file (concat temp-dir "/bookmarks"))
;; Decrease GC back
(add-hook 'after-init-hook
	  (lambda ()
	    (customize-set-variable 'gc-cons-threshold (* 2 1000 1000))))
;; increase the amount of data which Emacs reads from the process
(customize-set-variable 'read-process-output-max (* 1024 1024))
(fset 'yes-or-no-p 'y-or-n-p)
(windmove-default-keybindings)
;; desktop save
(customize-set-variable 'desktop-restore-frames t)
(customize-set-variable 'desktop-restore-reuses-frames t)
(customize-set-variable 'desktop-restore-forces-onscreen 'all)
(customize-set-variable 'desktop-restore-eager 16)
(customize-set-variable 'desktop-auto-save-time 16)
;; =============================================================================

;; Hooks
;; =============================================================================
(defun vs/--line-numbers ()
  "Display line numbers."
  (display-line-numbers-mode 1)
  (hl-line-mode 1))

(defun vs/--font-lock ()
  "Font lock keywords."
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

;; Enable ANSI colors on compilation mode
(require 'ansi-color)

(defun vs/--colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'vs/--line-numbers)
(add-hook 'text-mode-hook 'vs/--line-numbers)
(add-hook 'prog-mode-hook 'vs/--font-lock)
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'compilation-filter-hook
          #'vs/--colorize-compilation)

;; enable dired-find-alternate-file
(add-hook 'window-setup-hook
          (lambda ()
            (put 'dired-find-alternate-file 'disabled nil)))
;; =============================================================================

;; Enable vanilla modes
;; =============================================================================
(global-auto-revert-mode 1)
(show-paren-mode 1)
;; =============================================================================

;; Disable vanilla modes
;; =============================================================================
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(customize-set-variable 'inhibit-startup-screen t)
(customize-set-variable 'inhibit-splash-screen t)
;; =============================================================================

;; Server
;; =============================================================================
(require 'server)
(when (and (fboundp 'server-running-p)
           (not (server-running-p)))
  (server-start))
;; =============================================================================

(provide 'base)
;;; base.el ends here
