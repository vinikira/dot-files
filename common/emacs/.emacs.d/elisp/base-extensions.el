;;; package --- Base Extensions -*- lexical-binding: t -*-
;;; Commentary:
;;; All small extensions

;;; Code:

;; All the Icons
;; =============================================================================
(straight-use-package 'all-the-icons)

;; =============================================================================

;; Company mode
;; =============================================================================
(straight-use-package 'company)
(straight-use-package 'company-quickhelp)

(customize-set-variable 'company-idle-delay 0.25)
(customize-set-variable 'company-minimum-prefix-length 2)
(customize-set-variable 'company-tooltip-limit 14)
(customize-set-variable 'company-tooltip-align-annotations t)
(customize-set-variable 'company-require-match 'never)
(customize-set-variable 'company-global-modes
			'(not erc-mode message-mode help-mode gud-mode))
(customize-set-variable 'company-frontends '(company-pseudo-tooltip-frontend))
(customize-set-variable 'company-echo-metadata-frontend
			'(company-pseudo-tooltip-frontend
			  company-echo-metadata-frontend))
;; Buffer-local backends will be computed when loading a major mode, so
;; only specify a global default here.
(customize-set-variable 'company-backends '(company-capf))

;; These auto-complete the current selection when
;; `company-auto-complete-chars' is typed. This is too magical. We
;; already have the much more explicit RET and TAB.
(customize-set-variable 'company-auto-commit nil)
(customize-set-variable 'company-auto-commit-chars nil)

;; Only search the current buffer for `company-dabbrev' (a backend that
;; suggests text your open buffers). This prevents Company from causing
;; lag once you have a lot of buffers open.
(customize-set-variable 'company-dabbrev-other-buffers nil)
;; Make `company-dabbrev' fully case-sensitive, to improve UX with
;; domain-specific words with particular casing.
(customize-set-variable 'company-dabbrev-ignore-case nil)
(customize-set-variable 'company-dabbrev-downcase nil)

(global-set-key (kbd "C-.") 'company-complete)

(global-company-mode 1)
;; =============================================================================

;; Diff HL
;; =============================================================================
(straight-use-package 'diff-hl)

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(global-diff-hl-mode)
;; =============================================================================

;; Dump jump
;; =============================================================================
(straight-use-package 'dumb-jump)
(require 'xref)
(add-to-list 'xref-backend-functions #'dumb-jump-xref-activate)
;; =============================================================================

;; Editor config
;; =============================================================================
(straight-use-package 'editorconfig)
(editorconfig-mode 1)
;; =============================================================================

;; Exec Path From Shell
;; =============================================================================
(straight-use-package 'exec-path-from-shell)
(when (daemonp)
  (exec-path-from-shell-initialize))
;; =============================================================================

;; Flycheck
;; =============================================================================
(straight-use-package 'flycheck)

(global-flycheck-mode 1)
;; =============================================================================

;; Impostman
;; =============================================================================
(straight-use-package
 '(impostman :type git :host github :repo "flashcode/impostman" :branch "main"))
;; =============================================================================

;; Ivy + Counsel + Swiper
;; =============================================================================
(straight-use-package 'counsel)

(customize-set-variable 'ivy-use-virtual-buffers t)

(ivy-mode 1)
(counsel-mode 1)

(global-set-key (kbd "C-x s") 'swiper)
;; =============================================================================

;; Magit
;; =============================================================================
(straight-use-package 'magit)

(global-set-key (kbd "C-x g") 'magit-status)
;;=============================================================================

;; Multiple cursors
;; =============================================================================
(straight-use-package 'multiple-cursors)

(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c x") 'mc/mark-all-like-this)
;; =============================================================================

;; Password store
;; =============================================================================
(straight-use-package 'password-store)
;; =============================================================================

;; Projectile
;; =============================================================================
(straight-use-package 'projectile)

(customize-set-variable 'projectile-known-projects-file
			(expand-file-name "projectile-bookmarks.eld" temp-dir))

(customize-set-variable 'projectile-globally-ignored-directories
			'("node_modules" ".git" ".svn" "deps" "_build"))

(with-eval-after-load 'projectile
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(projectile-mode 1)
;; =============================================================================

;; Dashboard
;; =============================================================================
(straight-use-package 'dashboard)

(customize-set-variable 'dashboard-items '((recents  . 5)
					   (projects . 5)
					   (bookmarks . 5)
					   (agenda . 5)))
(customize-set-variable 'dashboard-set-file-icons t)
(customize-set-variable 'dashboard-set-heading-icons t)
(customize-set-variable 'dashboard-startup-banner 'logo)
(customize-set-variable 'dashboard-center-content t)
(customize-set-variable 'initial-buffer-choice
			(lambda () (get-buffer "*dashboard*")))

(dashboard-setup-startup-hook)
;; =============================================================================

;; Ripgrep
;; =============================================================================
(straight-use-package 'ripgrep)
;; =============================================================================

;; Smartparens
;; =============================================================================
(straight-use-package 'smartparens)

(with-eval-after-load 'smartparens
  (define-key smartparens-mode-map (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "C-(") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-{") 'sp-backward-barf-sexp))

(smartparens-global-mode t)
;; =============================================================================

;; Smex
;; =============================================================================
(straight-use-package 'smex)
;; =============================================================================

;; Undo tree
;; =============================================================================
(straight-use-package 'undo-tree)
(customize-set-variable 'undo-tree-auto-save-history nil)
(customize-set-variable 'undo-tree-history-directory-alist
			`(("." . ,(concat temp-dir "/undo/"))))

(global-undo-tree-mode 1)
;; =============================================================================

;; View Large Files
;; =============================================================================
(straight-use-package 'vlf)
;; =============================================================================

;; VTerm
;; =============================================================================
(straight-use-package 'vterm)
(global-set-key (kbd "<f7>") 'vterm-other-window)
;; =============================================================================

;; Wich Key
;; =============================================================================
(straight-use-package 'which-key)
(which-key-mode)
;; =============================================================================

;; XClip
;; =============================================================================
(straight-use-package 'xclip)
(xclip-mode)
;; =============================================================================

;; Yasnippet
;; =============================================================================
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(with-eval-after-load 'yasnippet
  (customize-set-variable
   'yas-snippet-dirs (append
		      yas-snippet-dirs
		      (list
		       (concat user-emacs-directory "snippets/")))))

(yas-global-mode 1)
;; =============================================================================

(provide 'base-extensions)

;;; base-extensions.el ends here
