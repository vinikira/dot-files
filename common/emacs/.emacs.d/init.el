;;; package --- Init file -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Profile emacs startup
;; =============================================================================
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))
;; =============================================================================

;; Straight
;; =============================================================================
(defvar bootstrap-version)
(customize-set-variable 'straight-check-for-modifications '(check-on-save find-when-checking))
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; =============================================================================

;; Byte compile files
;; =============================================================================
(byte-recompile-file (concat user-emacs-directory "early-init.el") 0)
(byte-recompile-file (concat user-emacs-directory "init.el") 0)
(byte-recompile-directory (concat user-emacs-directory "elisp") 0)
;; =============================================================================

;; Expand load-path with my elisp directory
;; =============================================================================
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
;; =============================================================================

;; Load customization layers
;; =============================================================================
(cl-loop for file in (reverse (directory-files-recursively
                               (concat user-emacs-directory "elisp") "\\.el$"))
         do (load (file-name-sans-extension file)))
;; =============================================================================

;;; init.el ends here
