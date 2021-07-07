;;; base-appearance.el --- Appearance Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Fonts Families
;; =============================================================================
(defvar vs/monospace-font-family "JetBrainsMono Nerd Font")
(defvar vs/monospace-serif-font-family "Noto Mono")
(defvar vs/sans-font-family "Noto Sans")
(defvar vs/emoji-font-family "Noto Color Emoji")

(defun vs/--safe-set-font (face font &optional height)
  "Set FONT to FACE if is installed.
If HEIGHT is non nil use it to set font heigth."
  (if (member font (font-family-list))
      (set-face-attribute face nil :family font :height (or height 100))
    (message "[set-font] Font %s not installed!" font)))

(defun vs/--safe-set-fontset (face font &optional add)
  "Set FONT as a fontset to FACE if is installed.
See `set-fontset-font' for ADD."
  (if (member font (font-family-list))
      (set-fontset-font t face font nil add)
    (message "[set-fontset] Font %s not installed!" font)))

(defun vs/--setup-fonts ()
  "Setup my fonts."
  (vs/--safe-set-font 'default vs/monospace-font-family 100)
  (vs/--safe-set-font 'fixed-pitch-serif vs/monospace-serif-font-family)
  (vs/--safe-set-font 'variable-pitch vs/sans-font-family)
  (vs/--safe-set-fontset 'symbol vs/emoji-font-family 'append))
;; =============================================================================

;; Theme
;; =============================================================================
(defun vs/--setup-theme ()
  "Configure theme."
  (load-theme 'modus-vivendi t))
;; =============================================================================

;; Setup frame
;; =============================================================================
(defvar vs/frame-alist
  `((scroll-bar . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (vertical-scroll-bars . nil)
    (fullscreen . maximized)
    (alpha . 100)))

(setq-default default-frame-alist vs/frame-alist)

(defun vs/--setup-frame ()
  "Configure frames."
  (vs/--setup-fonts)
  (vs/--setup-theme))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'vs/--setup-frame)
  (vs/--setup-frame))
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
                          ("\\*.*compilation.*\\*"
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
(customize-set-variable 'tab-bar-tab-name-function #'vs/--tab-bar-name)
(customize-set-variable 'tab-bar-new-tab-choice "*scratch*")
(customize-set-variable 'tab-bar-close-button-show nil)
(customize-set-variable 'tab-bar-new-button-show nil)

(global-set-key (kbd "H-t") #'tab-bar-select-tab-by-name)
;; =============================================================================

(provide 'base-appearance)

;;; base-appearance.el ends here
