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

(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "C-c o b") #'browse-url-of-file))

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

(add-hook 'web-mode-hook #'emmet-mode)
(add-hook 'vue-mode-hook #'emmet-mode)
(add-hook 'js-mode #'emmet-mode)

(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "C-c [") #'emmet-prev-edit-point)
  (define-key web-mode-map (kbd "C-c ]") #'emmet-next-edit-point))
;; =============================================================================

;; CSS Mode
;; =============================================================================
(straight-use-package 'css-mode)
;; =============================================================================

;; Pug Mode
;; =============================================================================
(straight-use-package 'pug-mode)
;; =============================================================================

;; Vue Mode
;; =============================================================================
(straight-use-package 'vue-mode)
;; =============================================================================

(provide 'layer-web)

;;; layer-web.el ends here
