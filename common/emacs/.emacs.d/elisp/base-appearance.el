;;; base-appearance.el --- Appearance Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Fonts Families
;; =============================================================================
(defvar vs/font-family "JetBrains Mono")
(defvar vs/emoji-font-family "Noto Color Emoji")
;; =============================================================================

;; Emoji font setup
;; More information: http://ergoemacs.org/emacs/emacs_list_and_set_font.html
;; =============================================================================
(defun vs/--setup-emoji-font (&rest _frame)
  "Set fontset emoji font for the FRAME."
  (set-fontset-font t 'symbol vs/emoji-font-family))

(unless (daemonp) (vs/--setup-emoji-font))

(advice-add 'server-create-window-system-frame :after 'vs/--setup-emoji-font)
;; =============================================================================

;; Theme
;; =============================================================================
(declare-function straight-use-package "ext:straight")

(straight-use-package 'dracula-theme)

(load-theme 'dracula t)
;; =============================================================================

;; Frame face
;; =============================================================================
(defvar vs/frame-alist
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
(declare-function all-the-icons-alltheicon "ext:all-the-icons")
(declare-function all-the-icons-octicon "ext:all-the-icons")
(declare-function all-the-icons-octicon-family "ext:all-the-icons")
(declare-function all-the-icons-icon-for-mode "ext:all-the-icons")
(declare-function all-the-icons-icon-family-for-buffer "ext:all-the-icons")

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
   " 🗓 "
   (:eval (format-time-string "%Y-%m-%d"))
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
(declare-function projectile-project-name "ext:projectile")

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
