;;; base-appearance.el --- Appearance Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Fonts Families
;; More information: http://ergoemacs.org/emacs/emacs_list_and_set_font.html
;; =============================================================================
(defconst vs/font-family "JetBrainsMono")
(defconst vs/emoji-font-family
  (cond
   ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
   ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
   ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
   ((member "Symbola" (font-family-list)) "Symbola")
   ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")))
;; =============================================================================

;; Font emoji setup
;; =============================================================================
(set-fontset-font t '(#x1f300 . #x1fad0) vs/emoji-font-family)
;; =============================================================================

;; Theme
;; =============================================================================
(straight-use-package 'dracula-theme)

(load-theme 'dracula t)
;; =============================================================================

;; Frame face
;; =============================================================================
(defconst vs/frame-alist
  `((font . ,(format "%s-11:width=regular:weight=regular" vs/font-family))
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
			   (window-height . 0.30)
			   (side . bottom)
			   (slot . -1))
			  ("\\*compilation\\*"
			   (display-buffer-in-side-window)
			   (window-height . 0.30)
			   (side . bottom)
			   (slot . 0))
			  ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Help\\)\\*"
			   (display-buffer-in-side-window)
			   (window-height . 0.30)
			   (side . bottom)
			   (slot . 1))))

;; =============================================================================

;; Tab bar mode
;; =============================================================================
(defun vs/--tab-bar-name ()
  "Custom function to generate tab bar names."
  (let ((project-name (projectile-project-name)))
    (if (string= "-" project-name)
	(tab-bar-tab-name-current)
      project-name)))

(customize-set-variable 'tab-bar-mode t)
(customize-set-variable 'tab-bar-show nil)
(customize-set-variable 'tab-bar-tab-name-function #'vs/--tab-bar-name)

(global-set-key (kbd "H-t") #'tab-bar-select-tab-by-name)
;; =============================================================================

(provide 'base-appearance)

;;; base-appearance.el ends here
