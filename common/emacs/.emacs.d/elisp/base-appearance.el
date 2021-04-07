;;; base-appearance.el --- Appearance Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Fonts Families
;; =============================================================================
(defconst vs/font-family
  (cond
   ((member "JetBrains Mono" (font-family-list)) "JetBrains Mono")
   ((member "DejaVu Sans Mono" (font-family-list)) "DejaVu Sans Mono")))

(defconst vs/emoji-font-family
  (cond
   ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
   ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
   ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
   ((member "Symbola" (font-family-list)) "Symbola")
   ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")))
;; =============================================================================

;; Font emoji setup
;; More information: http://ergoemacs.org/emacs/emacs_list_and_set_font.html
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
;; See more: https://github.com/domtronn/all-the-icons.el/wiki/Mode-Line
;; =============================================================================
(defun vs/--custom-modeline-git-vc ()
  "Define the custom icons for vc mode."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format "%s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
     " git "
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                 'display '(raise -0.1))
     (propertize (format " %s" branch) 'face `(:height 1.0)))))

(defun vs/--custom-modeline-mode-icon ()
  "Define the icon for current major mode."
  (format "%s"
	  (propertize (all-the-icons-icon-for-mode major-mode)
                      'help-echo (format "Major-mode: `%s`" major-mode)
                      'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))

(customize-set-variable
 'mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-modified
   mode-line-remote
   " 🕘 "
   (:eval (format-time-string "%H:%M"))
   " 📝 "
   "%l:%c"
   " · "
   (:eval (propertized-buffer-identification "%b"))
   " · "
   "("
   (:eval (vs/--custom-modeline-mode-icon))
   " "
   mode-name
   ")"
   " · "
   (:eval (when vc-mode (vs/--custom-modeline-git-vc)))))

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
