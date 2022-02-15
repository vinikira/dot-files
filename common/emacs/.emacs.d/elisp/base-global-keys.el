;;; base-global-keys.el --- Global Keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Global keys
;; =============================================================================
(declare-function vs/indent-buffer "base-functions")
(declare-function vs/split-window-below-and-switch "base-functions")
(declare-function vs/split-window-right-and-switch "base-functions")
(declare-function vs/move-line-up "base-functions")
(declare-function vs/move-line-down "base-functions")
(declare-function vs/duplicate-current-line "base-functions")
(declare-function vs/scratch-buffer "base-functions")

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
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
;; =============================================================================

;; Dired mode
;; =============================================================================
(with-eval-after-load 'dired
  (declare-function dired-create-empty-file "dired-aux")
  (declare-function dired-find-alternate-file "dired")

  (when (boundp 'dired-mode-map)
    (define-key dired-mode-map (kbd "e") #'dired-create-empty-file)
    (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)))
;; =============================================================================

;; Nxml mode
;; =============================================================================
(with-eval-after-load 'nxml-mode
  (declare-function vs/format-xml-buffer "base-functions")
  (declare-function vs/nxml-where "base-functions")

  (when (boundp 'nxml-mode-map)
    (define-key nxml-mode-map (kbd "C-c C-f") #'vs/format-xml-buffer)
    (define-key nxml-mode-map (kbd "C-c C-w") #'vs/nxml-where)))
;; =============================================================================

;; Unset keys
;; =============================================================================
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
;; =============================================================================

(provide 'base-global-keys)
;;; base-global-keys.el ends here
