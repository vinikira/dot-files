;;; base-appearance.el --- Appearance Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Theme
;; =============================================================================
(straight-use-package 'dracula-theme)

(load-theme 'dracula t)
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
   " ☰ "
   "%l:%c"
   " · "
   (:eval (propertized-buffer-identification "%b"))
   " · "
   "("
   mode-name
   ")"
   (:eval (when vc-mode (concat " » " (projectile-project-name) " ")))
   (vc-mode vc-mode)))
;; =============================================================================

(provide 'base-appearance)

;;; base-appearance.el ends here
