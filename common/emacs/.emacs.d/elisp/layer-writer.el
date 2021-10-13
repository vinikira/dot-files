;;; layer-writer.el --- Writer Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; LaTeX
;; =============================================================================
(straight-use-package 'auctex)
(straight-use-package 'auctex-latexmk)

(customize-set-variable 'auctex-latexmk-inherit-TeX-PDF-mode t)

(declare-function auctex-latexmk-setup "ext:auctex-latexmk")

(add-hook 'tex-mode-hook #'flyspell-mode)
(add-hook 'auctex-mode-hook #'auctex-latexmk-setup)
;; =============================================================================

;; Epub mode
;; =============================================================================
(straight-use-package 'nov)
(add-to-list 'auto-mode-alist '("\\.epub$" . nov-mode))
;; =============================================================================

;; Dark Room mode
;; =============================================================================
(straight-use-package 'darkroom)

(declare-function darkroom-tentative-mode "ext:darkroom")

(global-set-key (kbd "<f6>") #'darkroom-tentative-mode)
;; =============================================================================

;; Langtool
;; =============================================================================
(straight-use-package 'langtool)

(customize-set-variable 'langtool-default-language "en-US")

(with-eval-after-load 'langtool
  (declare-function find-lisp-find-files "find-lisp")
  (require 'find-lisp)
  (let* ((langtool-directory (if (boundp 'private-dir) private-dir "/tmp"))
         (langtool-link
          "https://languagetool.org/download/LanguageTool-stable.zip")
         (langtool-zip (concat langtool-directory "/langtool.zip"))
         (langtool-folder (concat langtool-directory "/langtool/")))
    (if (not (file-exists-p langtool-folder))
        (progn (message "Downloading langtool.zip")
               (async-shell-command
                (format "wget %s -O %s && unzip %s -d %s && rm %s"
                        langtool-link
                        langtool-zip
                        langtool-zip
                        langtool-folder
                        langtool-zip))))
    (customize-set-variable 'langtool-language-tool-jar
                            (car (find-lisp-find-files
                                  langtool-folder "languagetool-commandline.jar")))))
;; =============================================================================

(provide 'layer-writer)

;;; layer-writer.el ends here
