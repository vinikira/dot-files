;;; layer-config.el --- Config files layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; JSON
;; =============================================================================
(straight-use-package 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
;; =============================================================================

;; YAML
;; =============================================================================
(straight-use-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml|.yml$" . yaml-mode))
;; =============================================================================

;; CSV
;; =============================================================================
(straight-use-package 'csv-mode)

(add-to-list 'auto-mode-alist '("\\.csv$" . csv-mode))

(add-hook 'csv-mode-hook 'csv-align-mode)
;; =============================================================================

;; Ansible
;; =============================================================================
(straight-use-package 'ansible)
(straight-use-package 'ansible-doc)
(straight-use-package 'company-ansible)

(add-hook 'yaml-mode-hook 'ansible)
(add-hook 'yaml-mode-hook 'company-ansible)

(with-eval-after-load 'ansible
  (define-key ansible-key-map (kbd "C-c C-d") 'ansible-doc))

(with-eval-after-load 'ansible-doc
  (define-key ansible-doc-module-mode-map (kbd "C-c C-d") 'ansible-doc))
;; =============================================================================

;; Docker
;; =============================================================================
(straight-use-package 'dockerfile-mode)
(straight-use-package 'docker-compose-mode)
(straight-use-package 'docker)
(straight-use-package 'docker-tramp)

(add-to-list 'auto-mode-alist '("\\Dockerfile$" . dockerfile-mode))

(global-set-key (kbd "C-c d") 'docker)
;; =============================================================================

;; GraphQL
;; =============================================================================
(straight-use-package 'graphql-mode)
(add-to-list 'auto-mode-alist '("\\.graphql$|\\.gql$" . graphql-mode))
;; =============================================================================

;; HashiCorp Configuration Language
;; =============================================================================
(straight-use-package 'hcl-mode)
(add-to-list 'auto-mode-alist '("\\.hcl$|\\.tf$" . hcl-mode))
;; =============================================================================

;; Markdown
;; =============================================================================
(straight-use-package 'markdown-mode)
(straight-use-package 'markdownfmt)

(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(customize-set-variable 'markdown-command
			"pandoc --from markdown --to html --ascii")

(add-hook 'markdown-mode-hook 'markdownfmt-enable-on-save)

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-f") 'markdownfmt-format-buffer))
;; =============================================================================

;; Nginx
;; =============================================================================
(straight-use-package 'nginx-mode)
;; =============================================================================

;; Plantuml Mode
;; =============================================================================
(straight-use-package 'plantuml-mode)
(straight-use-package 'flycheck-plantuml)
(add-to-list 'auto-mode-alist '("\\.plantuml$|\\.puml$'" . plantuml-mode))

(customize-set-variable 'plantuml-output-type "svg")
(customize-set-variable 'plantuml-default-exec-mode 'jar)

(with-eval-after-load 'plantuml-mode
  (let* ((plantuml-directory private-dir)
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
  (require 'flycheck-plantuml)
  (flycheck-plantuml-setup))
;; =============================================================================

;; TOML Mode
;; =============================================================================
(straight-use-package 'toml-mode)
(add-to-list 'auto-mode-alist '("\\.toml$" . toml-mode))
;; =============================================================================

(provide 'layer-config)

;;; layer-config.el ends here
