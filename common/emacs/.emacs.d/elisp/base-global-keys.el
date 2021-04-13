;;; base-global-keys.el --- Global Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Set keys
;; =============================================================================
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "e") #'dired-create-empty-file)
  (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file))

(global-set-key (kbd "C-x C-b") #'ibuffer)
(global-set-key (kbd "C-c i") #'vs/indent-buffer)
(global-set-key (kbd "H-=") #'maximize-window)
(global-set-key (kbd "H--") #'minimize-window)
(global-set-key (kbd "H-0") #'balance-windows)
(global-set-key (kbd "C-x 2") #'vs/split-window-below-and-switch)
(global-set-key (kbd "C-x 3") #'vs/split-window-right-and-switch)
(global-set-key (kbd "M-S-<up>") #'vs/move-line-up)
(global-set-key (kbd "M-S-<down>") #'vs/move-line-down)
(global-set-key (kbd "H-d") #'vs/duplicate-current-line)
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "C-c s b") #'vs/scratch-buffer)
(global-set-key (kbd "<f8>") #'window-toggle-side-windows)
;; =============================================================================

;; Unset keys
;; =============================================================================
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
;; =============================================================================

(provide 'base-global-keys)
;;; base-global-keys.el ends here
