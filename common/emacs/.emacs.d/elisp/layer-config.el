;;; layer-config.el --- Config files layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; JSON
;; =============================================================================
(straight-use-package 'json-mode)
;; =============================================================================

;; YAML
;; =============================================================================
(straight-use-package 'yaml-mode)
;; =============================================================================

;; CSV
;; =============================================================================
(straight-use-package 'csv-mode)

(declare-function csv-align-mode "ext:csv-mode")

(add-hook 'csv-mode-hook #'csv-align-mode)
;; =============================================================================

;; Docker
;; =============================================================================
(straight-use-package 'dockerfile-mode)
(straight-use-package 'docker-compose-mode)
(straight-use-package 'docker)
(straight-use-package 'docker-tramp)

(declare-function docker "ext:docker")

(global-set-key (kbd "C-c d") #'docker)

(when (eq system-type 'darwin)
  (customize-set-variable 'docker-command "nerdctl.lima")
  (customize-set-variable 'docker-compose-command "nerdctl.lima compose")
  (customize-set-variable 'docker-tramp-docker-executable "nerdctl.lima"))
;; =============================================================================

;; GraphQL
;; =============================================================================
(straight-use-package 'graphql-mode)
;; =============================================================================

;; Graphviz
;; =============================================================================
(straight-use-package 'graphviz-dot-mode)

(customize-set-variable 'graphviz-dot-indent-width 4)
;; =============================================================================

;; HashiCorp Configuration Language
;; =============================================================================
(straight-use-package 'hcl-mode)
;; =============================================================================

;; Kubernetes
;; =============================================================================
(straight-use-package 'kubernetes-tramp)

(customize-set-variable 'tramp-remote-shell-executable "sh")
;; =============================================================================

;; Markdown
;; =============================================================================
(straight-use-package 'markdown-mode)
(straight-use-package 'markdownfmt)

(add-to-list 'auto-mode-alist '("README\\.md$" . gfm-mode))

(customize-set-variable 'markdown-command
                        "pandoc --quiet -f gfm -s")

(declare-function markdownfmt-enable-on-save "ext:markdownfmt")

(add-hook 'markdown-mode-hook #'markdownfmt-enable-on-save)
(add-hook 'gfm-mode #'markdownfmt-enable-on-save)

(with-eval-after-load 'markdown-mode
  (declare-function markdownfmt-format-buffer "ext:markdownfmt")
  (when (boundp 'markdown-mode-map)
    (define-key markdown-mode-map (kbd "C-c C-f") #'markdownfmt-format-buffer)))
;; =============================================================================

;; Mermaid Mode
;; =============================================================================
(straight-use-package 'mermaid-mode)
;; =============================================================================

;; Nginx
;; =============================================================================
(straight-use-package 'nginx-mode)
;; =============================================================================

;; Plantuml Mode
;; =============================================================================
(straight-use-package 'plantuml-mode)

(customize-set-variable 'plantuml-output-type "png")
(customize-set-variable 'plantuml-default-exec-mode 'jar)

(with-eval-after-load 'plantuml-mode
  (let* ((plantuml-directory (if (boundp 'private-dir) private-dir "/tmp"))
         (plantuml-link
          "http://sourceforge.net/projects/plantuml/files/plantuml.jar/download")
         (plantuml-target (concat plantuml-directory "/plantuml.jar")))
    (if (not (file-exists-p plantuml-target))
        (progn (message "Downloading plantuml.jar")
               (async-shell-command
                (format "wget %s -O %s" plantuml-link plantuml-target))))
    (customize-set-variable 'org-plantuml-jar-path plantuml-target)
    (customize-set-variable 'plantuml-jar-path plantuml-target)))
;; =============================================================================

;; Protobuf mode
;; =============================================================================
(straight-use-package 'protobuf-mode)
;; =============================================================================

;; TOML Mode
;; =============================================================================
(straight-use-package 'toml-mode)
;; =============================================================================

(provide 'layer-config)

;;; layer-config.el ends here
