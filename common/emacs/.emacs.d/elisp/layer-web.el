;;; layer-web.el --- Web Layer -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Web Mode
;; =============================================================================
(straight-use-package 'web-mode)

(customize-set-variable 'web-mode-markup-indent-offset 2)
(customize-set-variable 'web-mode-css-indent-offset 2)
(customize-set-variable 'web-mode-code-indent-offset 2)
(customize-set-variable 'web-mode-enable-current-element-highlight t)

(defun vs/--web-mode-hook ()
  "Hook for `web-mode' config for company-backends."
  (set (make-local-variable 'company-backends)
       '((company-css company-web-html company-files))))

(add-hook 'web-mode-hook 'vs/--web-mode-hook)

(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "C-c o b") 'browse-url-of-file))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.njk?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mjml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.eex\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.leex\\'" . web-mode))
;; =============================================================================

;; Emmet Mode
;; =============================================================================
(straight-use-package 'emmet-mode)

(customize-set-variable 'emmet-move-cursor-between-quotes t)

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'vue-mode-hook 'emmet-mode)
(add-hook 'rjsx-mode-hook 'emmet-mode)
(add-hook 'rjsx-minor-mode-hook 'emmet-mode)

(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "C-c [") 'emmet-prev-edit-point)
  (define-key web-mode-map (kbd "C-c ]") 'emmet-next-edit-point))
;; =============================================================================

;; CSS Mode
;; =============================================================================
(straight-use-package 'css-mode)

(defun vs/--css-mode-hook ()
  "Add company-css to company-backends."
  (set (make-local-variable 'company-backends)
       '((company-css company-dabbrev-code company-files))))

(add-hook 'css-mode-hook 'vs/--css-mode-hook)
;; =============================================================================

;; Company web
;; =============================================================================
(straight-use-package 'company-web)
;; =============================================================================

;; Pug Mode
;; =============================================================================
(straight-use-package 'pug-mode)
(add-to-list 'auto-mode-alist '("\\.pug?\\'" . pug-mode))
;; =============================================================================

;; React Mode
;; =============================================================================
(straight-use-package 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
;; =============================================================================

;; Vue Mode
;; =============================================================================
(straight-use-package 'vue-mode)
(add-to-list 'auto-mode-alist '("\\.vue$" . vue-mode))
;; =============================================================================

(provide 'layer-web)

;;; layer-web.el ends here
