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

;; Ansible
;; =============================================================================
(straight-use-package 'ansible)
(straight-use-package 'ansible-doc)

(add-hook 'yaml-mode-hook 'ansible)

(declare-function ansible-doc "ext:ansible-doc")

(with-eval-after-load 'ansible
  (when (boundp 'ansible-key-map)
    (define-key ansible-key-map (kbd "C-c C-d") #'ansible-doc)))

(with-eval-after-load 'ansible-doc
  (when (boundp 'ansible-doc-module-mode-map)
    (define-key ansible-doc-module-mode-map (kbd "C-c C-d") #'ansible-doc)))
;; =============================================================================

;; Docker
;; =============================================================================
(straight-use-package 'dockerfile-mode)
(straight-use-package 'docker-compose-mode)
(straight-use-package 'docker)
(straight-use-package 'docker-tramp)

(declare-function docker "ext:docker")

(global-set-key (kbd "C-c d") #'docker)
;; =============================================================================

;; GraphQL
;; =============================================================================
(straight-use-package 'graphql-mode)
;; =============================================================================

;; Graphviz
;; =============================================================================
(straight-use-package 'graphviz-dot-mode)

(with-eval-after-load 'graphviz-dot-mode
  (customize-set-variable 'graphviz-dot-indent-width 4))
;; =============================================================================

;; HashiCorp Configuration Language
;; =============================================================================
(straight-use-package 'hcl-mode)
;; =============================================================================

;; Markdown
;; =============================================================================
(straight-use-package 'markdown-mode)
(straight-use-package 'markdownfmt)

(add-to-list 'auto-mode-alist '("README\\.md$" . gfm-mode))

(customize-set-variable 'markdown-command
                        "pandoc --from markdown --to html --ascii")

(declare-function markdownfmt-enable-on-save "ext:markdownfmt")

(add-hook 'markdown-mode-hook #'markdownfmt-enable-on-save)
(add-hook 'gfm-mode #'markdownfmt-enable-on-save)

(with-eval-after-load 'markdown-mode
  (declare-function markdownfmt-format-buffer "ext:markdownfmt")
  (when (boundp 'markdown-mode-map)
    (define-key markdown-mode-map (kbd "C-c C-f") #'markdownfmt-format-buffer)))
;; =============================================================================

;; Nginx
;; =============================================================================
(straight-use-package 'nginx-mode)
;; =============================================================================

;; Plantuml Mode
;; =============================================================================
(straight-use-package 'plantuml-mode)
(straight-use-package 'flycheck-plantuml)

(customize-set-variable 'plantuml-output-type "svg")
(customize-set-variable 'plantuml-default-exec-mode 'jar)

(with-eval-after-load 'plantuml-mode
  (declare-function flycheck-plantuml-setup "ext:flycheck-plantuml")
  (let* ((plantuml-directory (if (boundp 'private-dir) private-dir "/tmp"))
         (plantuml-link
          "http://sourceforge.net/projects/plantuml/files/plantuml.jar/download")
         (plantuml-target (concat plantuml-directory "/plantuml.jar")))
    (if (not (file-exists-p plantuml-target))
        (progn (message "Downloading plantuml.jar")
               (shell-command
                (format "wget %s -O %s" plantuml-link plantuml-target))
               (kill-buffer "*Shell Command Output*")))
    (customize-set-variable 'org-plantuml-jar-path plantuml-target)
    (customize-set-variable 'plantuml-jar-path plantuml-target))
  (flycheck-plantuml-setup))
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
