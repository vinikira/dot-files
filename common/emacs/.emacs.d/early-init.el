;;; early-init.el --- Early init Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Disable garbage collection on startup
(setq gc-cons-threshold most-positive-fixnum)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Frame face
(defconst vs/frame-alist
  '((font . "JetBrainsMono-11:width=regular:weight=regular")
    (scroll-bar . 0)
    (menu-bar-lines . 0)
    (vertical-scroll-bars)
    (height . 60)
    (width . 95)
    (alpha . 100)))

(setq-default default-frame-alist vs/frame-alist)
;;; early-init.el ends here
