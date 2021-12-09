;;; lang-elixir.el --- Elixir layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Elixir mode
;; =============================================================================
(straight-use-package 'elixir-mode)

(with-eval-after-load 'elixir-mode
  (declare-function elixir-format "ext:elixir-format")

  (when (boundp 'elixir-mode-map)
    (define-key elixir-mode-map (kbd "C-c C-f") #'elixir-format)))

(add-to-list 'auto-mode-alist '("\\mix.lock$" . elixir-mode))
;; =============================================================================

;; Ex Unit
;; =============================================================================
(straight-use-package 'exunit)

(with-eval-after-load 'elixir-mode
  (declare-function exunit-verify-all "ext:exunit")
  (declare-function exunit-verify-all-in-umbrella "ext:exunit")
  (declare-function exunit-verify-single "ext:exunit")
  (declare-function exunit-verify "ext:exunit")
  (declare-function exunit-rerun "ext:exunit")
  (declare-function exunit-toggle-file-and-test "ext:exunit")
  (declare-function exunit-toggle-file-and-test-other-window "ext:exunit")

  (when (boundp 'elixir-mode-map)
    (define-key elixir-mode-map (kbd "C-c , a") #'exunit-verify-all)
    (define-key elixir-mode-map (kbd "C-c , A") #'exunit-verify-all-in-umbrella)
    (define-key elixir-mode-map (kbd "C-c , s") #'exunit-verify-single)
    (define-key elixir-mode-map (kbd "C-c , v") #'exunit-verify)
    (define-key elixir-mode-map (kbd "C-c , r") #'exunit-rerun)
    (define-key elixir-mode-map (kbd "C-c , t") #'exunit-toggle-file-and-test)
    (define-key elixir-mode-map (kbd "C-c , T") #'exunit-toggle-file-and-test-other-window)))

(defvar vs/use-exunit-custom nil
  "If true uses custom command stored in `vs/exunit-custom-mix-cmd'.")

(defvar vs/exunit-custom-mix-cmd "mix test %s"
  "Custom exunit command, must indicate the args position with %s.")

(with-eval-after-load 'exunit
  (declare-function s-join "ext:s")
  (declare-function exunit-do-compile "ext:exunit")
  (declare-function exunit-project-root "ext:exunit")
  (declare-function s-join "ext:s")

  (defvar exunit-environment)
  (defvar exunit-mix-test-default-options)

  (advice-add 'exunit-compile :around
              (lambda (orig-fun &rest args)
                "Apply ARGS to ORIG-FUN if vs/use-exunit-custom is nil."
                (if vs/use-exunit-custom
                    (let* ((directory (cadr args))
                           (default-directory (or directory (exunit-project-root)))
                           (compilation-environment exunit-environment)
                           (args (car args)))
                      (ignore default-directory) ;; avoiding unused lexical variable warning
                      (ignore compilation-environment) ;; avoiding unused lexical variable warning
                      (exunit-do-compile
                       (format vs/exunit-custom-mix-cmd
                               (s-join " " (append exunit-mix-test-default-options args)))))
                  (apply orig-fun args)))))
;; =============================================================================

;; Inf Elixir
;; =============================================================================
(straight-use-package
 '(inf-elixir :type git :host github :repo "vinikira/inf-elixir.el" :branch "main"))
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

;; Helpers
;; =============================================================================
(defun vs/json-to-etf (&optional begin end)
  "Transform JSON to Elixir Term Format.  Use BEGIN and END as region."
  (interactive "r")
  (save-excursion
    (let ((min (if (region-active-p) begin (point-min))))
      (goto-char min)
      (while (re-search-forward ":" end t)
        (replace-match " =>"))
      (goto-char min)
      (while (re-search-forward "{" end t)
        (replace-match "%{")))))

(defun vs/etf-to-json (&optional begin end)
  "Transform Elixir Term Format to JSON.  Use BEGIN and END as region."
  (interactive "r")
  (save-excursion
    (let ((min (if (region-active-p) begin (point-min))))
      (goto-char min)
      (while (re-search-forward " =>" end t)
        (replace-match ":"))
      (goto-char min)
      (while (re-search-forward "%{" end t)
        (replace-match "{")))))

(defun vs/elixir-map-atom-to-map-string (&optional begin end)
  "Transform Elixir map atom to map string.  Use BEGIN and END as region."
  (interactive "r")
  (save-excursion
    (let ((min (if (region-active-p) begin (point-min))))
      (goto-char min)
      (while (re-search-forward "\\([a-zA-z0-9]+\\): " end t)
        (replace-match "\"\\1\" => "))
      (goto-char min))))

(defun vs/elixir-map-string-to-map-atom (&optional begin end)
  "Transform Elixir map string to map atom.  Use BEGIN and END as region."
  (interactive "r")
  (save-excursion
    (let ((min (if (region-active-p) begin (point-min))))
      (goto-char min)
      (while (re-search-forward "\"\\([a-zA-z0-9]+\\)\" => " end t)
        (replace-match "\\1: "))
      (goto-char min))))
;; =============================================================================

(provide 'lang-elixir)

;;; lang-elixir.el ends here
