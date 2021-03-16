;;; base-appearance.el --- Appearance Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Theme
;; =============================================================================
(straight-use-package 'dracula-theme)

(load-theme 'dracula t)
;; =============================================================================

;; Frame face
;; =============================================================================
(defconst vs/frame-alist
  '((font . "JetBrainsMono-11:width=regular:weight=regular")
    (scroll-bar . 0)
    (menu-bar-lines . 0)
    (vertical-scroll-bars)
    (height . 60)
    (width . 95)
    (alpha . 100)))

(setq-default default-frame-alist vs/frame-alist)
;; =============================================================================

;; Modeline
;; =============================================================================
(customize-set-variable
 'mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-modified
   mode-line-remote
   " ⌚ "
   (:eval (format-time-string "%H:%M"))
   "  "
   "%l:%c"
   " · "
   (:eval (propertized-buffer-identification "%b"))
   " · "
   "("
   (:eval (all-the-icons-icon-for-mode major-mode))
   " "
   mode-name
   ")"
   (:eval (when vc-mode
	    (concat " "
		    (all-the-icons-icon-for-dir (projectile-project-root))
		    " "
		    (projectile-project-name)
		    " ")))
   (vc-mode vc-mode)))

;; =============================================================================

;; Side windows
;; =============================================================================
(customize-set-variable 'display-buffer-alist
			'(("\\(vterm\\|\\*vterm\\*\\|\\*e?shell\\*\\)"
			   (display-buffer-in-side-window)
			   (window-height . 0.25)
			   (side . bottom)
			   (slot . -1))
			  ("\\(*exunit-compilation*\\|*compilation*\\)"
			   (display-buffer-in-side-window)
			   (window-height . 0.25)
			   (side . bottom)
			   (slot . 0))
			  ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
			   (display-buffer-in-side-window)
			   (window-height . 0.25)
			   (side . bottom)
			   (slot . 1)
			   (window-parameters . ((no-other-window . t))))))

;; =============================================================================

;; Tab bar mode
;; =============================================================================
(defun vs/--tab-bar-name ()
  "Custom function to generate tab bar names."
  (if (not (equal (projectile-project-name) "-"))
      (projectile-project-name)
    (tab-bar-tab-name-current)))

(customize-set-variable 'tab-bar-tab-name-function #'vs/--tab-bar-name)

(set-face-attribute 'tab-bar-tab nil :family "JetBrains Mono")
(set-face-attribute 'tab-bar-tab-inactive nil :family "JetBrains Mono")
;; =============================================================================

(provide 'base-appearance)

;;; base-appearance.el ends here
