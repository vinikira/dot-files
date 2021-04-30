;;; early-init.el --- Early init Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(let ((gc-cp gc-cons-percentage)
      (fha file-name-handler-alist))
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold (* 2 1000 1000) ;; 2MB
                    gc-cons-percentage gc-cp
                    file-name-handler-alist fha))))

;; Disable garbage collection on startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8
      file-name-handler-alist nil)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
