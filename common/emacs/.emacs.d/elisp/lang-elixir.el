;;; lang-elixir.el --- Elixir layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Elixir mode
;; =============================================================================
(straight-use-package 'elixir-mode)

(add-to-list 'auto-mode-alist '("\\.mix.lock$" . elixir-mode))
;; =============================================================================

;; Ex Unit
;; =============================================================================
(straight-use-package 'exunit)

(with-eval-after-load 'elixir-mode
  (define-key elixir-mode-map (kbd "C-c , a") #'exunit-verify-all)
  (define-key elixir-mode-map (kbd "C-c , A") #'exunit-verify-all-in-umbrella)
  (define-key elixir-mode-map (kbd "C-c , s") #'exunit-verify-single)
  (define-key elixir-mode-map (kbd "C-c , v") #'exunit-verify)
  (define-key elixir-mode-map (kbd "C-c , r") #'exunit-rerun))

;; =============================================================================

;; Inf Elixir
;; =============================================================================
(straight-use-package
 '(inf-elixir :type git :host github :repo "J3RN/inf-elixir" :branch "master"))

(with-eval-after-load 'elixir-mode
  (define-key elixir-mode-map (kbd "C-x C-e") #'inf-elixir-send-line)
  (define-key elixir-mode-map (kbd "C-M-x") #'inf-elixir-send-region)
  (define-key elixir-mode-map (kbd "C-S-M-x") #'inf-elixir-send-buffer))
;; =============================================================================

;; Poly mode for ~L sigil
;; =============================================================================
;; (straight-use-package 'polymode)

;; (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)

;; (define-innermode poly-liveview-expr-elixir-innermode
;;   :mode 'web-mode
;;   :head-matcher (rx line-start (* space) "~L" (= 3 (char "\"'")) line-end)
;;   :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
;;   :head-mode 'host
;;   :tail-mode 'host
;;   :allow-nested nil
;;   :keep-in-mode 'host
;;   :fallback-mode 'host)

;; (define-polymode poly-elixir-web-mode
;;   :hostmode 'poly-elixir-hostmode
;;   :innermodes '(poly-liveview-expr-elixir-innermode))

;; (with-eval-after-load 'web-mode
;;   (customize-set-variable 'web-mode-engines-alist '("elixir" . "\\.ex\\'")))

;; (add-to-list 'auto-mode-alist '("\\.ex$" . poly-elixir-web-mode))
;; =============================================================================

;; Org Babel Elixir
;; =============================================================================
(straight-use-package 'ob-elixir)
;; =============================================================================

(provide 'lang-elixir)

;;; lang-elixir.el ends here
