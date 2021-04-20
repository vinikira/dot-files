;;; package --- Base Extensions -*- lexical-binding: t -*-
;;; Commentary:
;;; All small extensions

;;; Code:

(declare-function straight-use-package "ext:straight")

;; All the Icons
;; =============================================================================
(straight-use-package 'all-the-icons)
;; =============================================================================

;; Diff HL
;; =============================================================================
(straight-use-package 'diff-hl)

(declare-function diff-hl-magit-pre-refresh "ext:diff-hl")
(declare-function diff-hl-magit-post-refresh "ext:diff-hl")
(declare-function global-diff-hl-mode "ext:diff-hl")

(add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
(add-hook 'after-init-hook #'global-diff-hl-mode)
;; =============================================================================

;; Dump jump
;; =============================================================================
(straight-use-package 'dumb-jump)

(with-eval-after-load 'xref
  (declare-function dumb-jump-xref-activate "ext:dumb-jump")
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
;; =============================================================================

;; Editor config
;; =============================================================================
(straight-use-package 'editorconfig)

(declare-function editorconfig-mode "ext:editorconfig")

(add-hook 'after-init-hook #'editorconfig-mode)
;; =============================================================================

;; Embark
;; =============================================================================
(straight-use-package 'embark)

(declare-function embark-act "ext:embark")
(declare-function embark-bindings "ext:embark")
(declare-function embark-act "ext:embark")

(global-set-key (kbd "M-o") #'embark-act)
(global-set-key (kbd "C-h B") #'embark-bindings)

(with-eval-after-load 'embark
  (declare-function embark-prefix-help-command "ext:embark")
  (declare-function which-key--show-keymap "ext:wich-key")
  (declare-function which-key--hide-popup-ignore-command "ext:wich-key")

  (customize-set-variable 'prefix-help-command #'embark-prefix-help-command)
  (customize-set-variable 'embark-action-indicator
                          (lambda (map _target)
                            (which-key--show-keymap "Embark" map nil nil 'no-paging)
                            #'which-key--hide-popup-ignore-command))
  (customize-set-variable 'embark-become-indicator 'embark-action-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; =============================================================================

;; Exec Path From Shell
;; =============================================================================
(straight-use-package 'exec-path-from-shell)

(declare-function exec-path-from-shell-initialize "ext:exec-path-from-shell")

(when (daemonp)
  (add-hook 'after-init-hook #'exec-path-from-shell-initialize))
;; =============================================================================

;; Flycheck
;; =============================================================================
(straight-use-package 'flycheck)

(declare-function flycheck-mode "ext:flycheck")

(add-hook 'prog-mode-hook #'flycheck-mode)
;; =============================================================================

;; Iedit
;; =============================================================================
(straight-use-package 'iedit)

(declare-function iedit-mode "ext:iedit")

(global-set-key (kbd "C-;") #'iedit-mode)
;; =============================================================================

;; Impostman
;; =============================================================================
(straight-use-package
 '(impostman :type git :host github :repo "flashcode/impostman" :branch "main"))
;; =============================================================================

;; Magit
;; =============================================================================
(straight-use-package 'magit)

(declare-function magit-status "ext:magit-status")

(global-set-key (kbd "C-x g") #'magit-status)
;; =============================================================================

;; Marginalia
;; =============================================================================
(straight-use-package 'marginalia)

(declare-function marginalia-mode "ext:marginalia")
(declare-function marginalia-cycle "ext:marginalia")

(with-eval-after-load 'marginalia
  (define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle))

(marginalia-mode)
;; =============================================================================

;; Multiple cursors
;; =============================================================================
(straight-use-package 'multiple-cursors)

(declare-function mc/mark-next-like-this "ext:mc-mark-more")
(declare-function mc/mark-previous-like-this "ext:mc-mark-more")
(declare-function mc/mark-all-like-this "ext:mc-mark-more")

(global-set-key (kbd "M-n") #'mc/mark-next-like-this)
(global-set-key (kbd "M-p") #'mc/mark-previous-like-this)
(global-set-key (kbd "C-c x") #'mc/mark-all-like-this)
;; =============================================================================

;; Password store
;; =============================================================================
(straight-use-package 'password-store)
;; =============================================================================

;; Projectile
;; =============================================================================
(straight-use-package 'projectile)

(declare-function projectile-mode "ext:projectile")

(with-eval-after-load 'projectile
  (declare-function projectile-command-map "ext:projectile")

  (when (boundp 'temp-dir)
    (customize-set-variable 'projectile-known-projects-file
                            (expand-file-name "projectile-bookmarks.eld" temp-dir)))

  (customize-set-variable 'projectile-globally-ignored-directories
                          '("node_modules" ".git" ".svn" "deps" "_build" ".elixir_ls"))

  (when (and (boundp 'projectile-mode-map))
    (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)))

(projectile-mode)
;; =============================================================================

;; Dashboard
;; =============================================================================
(straight-use-package 'dashboard)

(declare-function dashboard-setup-startup-hook "ext:dashboard")

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
(straight-use-package 'rg)

(declare-function rg-menu "ext:rg-menu")

(global-set-key (kbd "C-c r") #'rg-menu)
;; =============================================================================

;; Selectrum
;; =============================================================================
(straight-use-package 'selectrum)
(straight-use-package 'selectrum-prescient)

(declare-function selectrum-prescient-mode "ext:selectrum-prescient")
(declare-function prescient-persist-mode "ext:prescient")
(declare-function selectrum-mode "ext:selectrum")

(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
(selectrum-mode +1)
;; =============================================================================

;; Smartparens
;; =============================================================================
(straight-use-package 'smartparens)

(declare-function smartparens-mode "ext:smartparens")

(with-eval-after-load 'smartparens
  (declare-function sp-forward-slurp-sexp "ext:smartparens")
  (declare-function sp-forward-barf-sexp "ext:smartparens")
  (declare-function sp-backward-slurp-sexp "ext:smartparens")
  (declare-function sp-backward-barf-sexp "ext:smartparens")

  (with-eval-after-load 'prog-mode
    (require 'smartparens-config))

  (when (boundp 'smartparens-mode-map)
    (define-key smartparens-mode-map (kbd "C-)") #'sp-forward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-(") #'sp-forward-barf-sexp)
    (define-key smartparens-mode-map (kbd "C-{") #'sp-backward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-{") #'sp-backward-barf-sexp)))

(add-hook 'prog-mode-hook #'smartparens-mode)
;; =============================================================================

;; Smex
;; =============================================================================
(straight-use-package 'smex)
;; =============================================================================

;; Undo tree
;; =============================================================================
(straight-use-package 'undo-tree)

(declare-function global-undo-tree-mode "ext:undo-tree")

(customize-set-variable 'undo-tree-auto-save-history nil)

(when (boundp 'temp-dir)
  (customize-set-variable 'undo-tree-history-directory-alist
                          `(("." . ,(concat temp-dir "/undo/")))))

(add-hook 'after-init-hook #'global-undo-tree-mode)
;; =============================================================================

;; View Large Files
;; =============================================================================
(straight-use-package 'vlf)
;; =============================================================================

;; VTerm
;; =============================================================================
(straight-use-package 'vterm)

(declare-function vterm-other-window "ext:vterm")

(global-set-key (kbd "<f7>") #'vterm-other-window)
;; =============================================================================

;; Wich Key
;; =============================================================================
(straight-use-package 'which-key)

(declare-function which-key-mode "ext:which-key")

(add-hook 'after-init-hook #'which-key-mode)
;; =============================================================================

;; XClip
;; =============================================================================
(straight-use-package 'xclip)

(declare-function xclip-mode "ext:xclip")

(add-hook 'after-init-hook #'xclip-mode)
;; =============================================================================

;; Yasnippet
;; =============================================================================
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

(declare-function yas-global-mode "ext:yasnippet")

(with-eval-after-load 'yasnippet
  (when (boundp 'yas-snippet-dirs)
    (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets/"))))

(add-hook 'after-init-hook #'yas-global-mode)
;; =============================================================================

(provide 'base-extensions)

;;; base-extensions.el ends here
