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

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq-default native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happens asynchronously
  (setq native-comp-deferred-compilation t))

;; Disable garbage collection on startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8
      file-name-handler-alist nil)

;; Remove some unneeded UI elements (the user can turn back on anything they wish)
(setq inhibit-startup-message t)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(background-color . "#232635") default-frame-alist)
(push '(foreground-color . "#FFFFFF") default-frame-alist)
(push '(mouse-color . "white") default-frame-alist)

;; Disable package.el
(setq package-enable-at-startup nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
