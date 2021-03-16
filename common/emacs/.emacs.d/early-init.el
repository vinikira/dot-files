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

;;; early-init.el ends here
