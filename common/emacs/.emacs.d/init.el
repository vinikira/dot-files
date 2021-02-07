;;; package --- Init file -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Straight
;; =============================================================================
(defvar bootstrap-version)
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

;; Expand Load Path
;; =============================================================================
(add-to-list 'load-path (concat user-emacs-directory "elisp"))
;; =============================================================================

;; Customization Layers
;; =============================================================================
(require 'base)
(require 'base-functions)
(require 'base-global-keys)
(require 'base-appearance)
(require 'base-extensions)
(require 'layer-web)
(require 'lang-clojure)
(require 'lang-csharp)
(require 'lang-dart)
(require 'lang-elixir)
(require 'lang-elm)
(require 'lang-erlang)
(require 'lang-go)
(require 'lang-groovy)
(require 'lang-haskell)
(require 'lang-java)
(require 'lang-javascript)
(require 'lang-kotlin)
(require 'lang-lisp)
(require 'lang-python)
(require 'lang-rust)
(require 'layer-config)
(require 'layer-org)
(require 'layer-web)
(require 'layer-lsp)
(require 'layer-writer)
;; =============================================================================

;;; init.el ends here
