;;; base-completions.el --- Completions tweaks -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Selectrum - incremental narrowing
;; =============================================================================
(straight-use-package 'selectrum)

(declare-function selectrum-mode "ext:selectrum")

(with-eval-after-load 'selectrum
  (set-face-background 'selectrum-current-candidate "#44475a"))

(selectrum-mode +1)
;; =============================================================================

;; Prescient - Sorting and filtering
;; =============================================================================
(straight-use-package 'selectrum-prescient)

(declare-function selectrum-prescient-mode "ext:selectrum-prescient")
(declare-function prescient-persist-mode "ext:prescient")

(selectrum-prescient-mode +1)
(prescient-persist-mode +1)
;; =============================================================================

;; Embark - minibuffer actions
;; =============================================================================
(straight-use-package 'embark)

(declare-function embark-act "ext:embark")
(declare-function embark-bindings "ext:embark")
(declare-function embark-act "ext:embark")

(global-set-key (kbd "M-o") #'embark-act)
(global-set-key (kbd "C-h B") #'embark-bindings)

(with-eval-after-load 'embark
  (declare-function embark-prefix-help-command "ext:embark")
  (declare-function which-key--show-keymap "ext:wich-key")
  (declare-function which-key--hide-popup-ignore-command "ext:wich-key")

  (customize-set-variable 'prefix-help-command #'embark-prefix-help-command)
  (customize-set-variable 'embark-action-indicator
                          (lambda (map _target)
                            (which-key--show-keymap "Embark" map nil nil 'no-paging)
                            #'which-key--hide-popup-ignore-command))
  (customize-set-variable 'embark-become-indicator 'embark-action-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
;; =============================================================================

;; Marginalia - minibuffer annotations
;; =============================================================================
(straight-use-package 'marginalia)

(declare-function marginalia-mode "ext:marginalia")
(declare-function marginalia-cycle "ext:marginalia")

(with-eval-after-load 'marginalia
  (define-key minibuffer-local-map (kbd "M-A") #'marginalia-cycle))

(marginalia-mode)
;; =============================================================================

;; Consult - commands based on completing-read
;; =============================================================================
(straight-use-package 'consult)
(straight-use-package 'embark-consult)

(declare-function consult-history "ext:consult")
(declare-function consult-mode-command "ext:consult")
(declare-function consult-bookmark "ext:consult")
(declare-function consult-kmacro "ext:consult")

(global-set-key (kbd "C-c h") #'consult-history)
(global-set-key (kbd "C-c m") #'consult-mode-command)
(global-set-key (kbd "C-c b") #'consult-bookmark)
(global-set-key (kbd "C-c k") #'consult-kmacro)

(declare-function consult-complex-command "ext:consult")
(declare-function consult-buffer "ext:consult")
(declare-function consult-buffer-other-window "ext:consult")
(declare-function consult-buffer-other-frame "ext:consult")

(global-set-key (kbd "C-x M-:") #'consult-complex-command)
(global-set-key (kbd "C-x b") #'consult-buffer)
(global-set-key (kbd "C-x 4 b") #'consult-buffer-other-window)
(global-set-key (kbd "C-x 5 b") #'consult-buffer-other-frame)

;; Custom M-# bindings for fast register access
(declare-function consult-register-load "consult")
(declare-function consult-register-store "consult")
(declare-function consult-register "consult")

(global-set-key (kbd "M-#") #'consult-register-load)
(global-set-key (kbd "M-'") #'consult-register-store)
(global-set-key (kbd "C-M-#") #'consult-register)

;; Other custom bindings
(declare-function consult-yank-pop "ext:consult")
(declare-function consult-apropos "ext:consult")

(global-set-key (kbd "M-y") #'consult-yank-pop)
(global-set-key (kbd "<help> a") #'consult-apropos)

;; M-g bindings (goto-map)
(declare-function consult-compile-error "ext:consult-compile")
(declare-function consult-goto-line "ext:consult")
(declare-function consult-outline "ext:consult")
(declare-function consult-mark "ext:consult")
(declare-function consult-global-mark "ext:consult")
(declare-function consult-imenu "ext:consult")
(declare-function consult-project-imenu "ext:consult")

(global-set-key (kbd "M-g e") #'consult-compile-error)
(global-set-key (kbd "M-g g") #'consult-goto-line)
(global-set-key (kbd "M-g M-g") #'consult-goto-line)
(global-set-key (kbd "M-g o") #'consult-outline)
(global-set-key (kbd "M-g m") #'consult-mark)
(global-set-key (kbd "M-g k") #'consult-global-mark)
(global-set-key (kbd "M-g i") #'consult-imenu)
(global-set-key (kbd "M-g I") #'consult-project-imenu)

;; M-s bindings (search-map)
(declare-function consult-find "ext:consult")
(declare-function consult-locate "ext:consult")
(declare-function consult-grep "ext:consult")
(declare-function consult-git-grep "ext:consult")
(declare-function consult-ripgrep "ext:consult")
(declare-function consult-line "ext:consult")
(declare-function consult-multi-occur "ext:consult")
(declare-function consult-keep-lines "ext:consult")
(declare-function consult-focus-lines "ext:consult")

(global-set-key (kbd "M-s f") #'consult-find)
(global-set-key (kbd "M-s L") #'consult-locate)
(global-set-key (kbd "M-s g") #'consult-grep)
(global-set-key (kbd "M-s G") #'consult-git-grep)
(global-set-key (kbd "M-s r") #'consult-ripgrep)
(global-set-key (kbd "M-s l") #'consult-line)
(global-set-key (kbd "M-s m") #'consult-multi-occur)
(global-set-key (kbd "M-s k") #'consult-keep-lines)
(global-set-key (kbd "M-s u") #'consult-focus-lines)

;; Isearch integration
(declare-function consult-isearch "ext:consult")
(declare-function consult-line "ext:consult")

(global-set-key (kbd "M-s e") #'consult-isearch)
(define-key isearch-mode-map (kbd "M-e") #'consult-isearch)
(define-key isearch-mode-map (kbd "M-s e") #'consult-isearch)
(define-key isearch-mode-map (kbd "M-s l") #'consult-line)

(autoload 'projectile-project-root "projectile")
(customize-set-variable 'consult-project-root-function #'projectile-project-root)
;; =============================================================================

(provide 'base-completions)

;;; base-completions.el ends here
