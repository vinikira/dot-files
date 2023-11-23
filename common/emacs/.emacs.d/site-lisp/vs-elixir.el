;;; vs-elixir.el --- Elixir helpers -*- lexical-binding: t -*-

;; Author: Vinícius Simões
;; Maintainer: Vinícius Simões
;; Version: 0.0.1
;; Package-Requires: (elixir-mode project xwidget)
;; Homepage: homepage
;; Keywords: Elixir Emacs


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'project)

;;;###autoload
(defun vs/json-to-etf (&optional begin end)
  "Transform JSON to Elixir Term Format.  Use BEGIN and END as region."
  (interactive "r")
  (save-excursion
    (replace-regexp-in-region "\": " "\" => " begin end)
    (replace-regexp-in-region "{" "%{" begin end)
    (replace-regexp-in-region "null" "nil" begin end)))

;;;###autoload
(defun vs/etf-to-json (&optional begin end)
  "Transform Elixir Term Format to JSON.  Use BEGIN and END as region."
  (interactive "r")
  (save-excursion
    (replace-regexp-in-region "\" => " "\": " begin end)
    (replace-regexp-in-region "%{" "{" begin end)
    (replace-regexp-in-region "nil" "null" begin end)))

;;;###autoload
(defun vs/elixir-map-atom-to-map-string (&optional begin end)
  "Transform Elixir map atom to map string.  Use BEGIN and END as region."
  (interactive "r")
  (save-excursion
    (replace-regexp-in-region "\\([a-zA-z0-9]+\\): " "\"\\1\" => " begin end)))

;;;###autoload
(defun vs/elixir-map-string-to-map-atom (&optional begin end)
  "Transform Elixir map string to map atom.  Use BEGIN and END as region."
  (interactive "r")
  (save-excursion
    (replace-regexp-in-region "\"\\([a-zA-z0-9]+\\)\" =>" "\\1: " begin end)))

;;;###autoload
(defun vs/elixir-open-dep-docs (&optional force-external)
  "Open the choosen dep in hexdocs using xwidget-webkit if available.
FORCE-EXTERNAL browser if provided."
  (interactive "P")
  (unless (project-current)
    (user-error "Not in a project"))
  (let* ((default-directory (project-root (project-current)))
         (url
          (with-temp-buffer
            (insert (shell-command-to-string "mix deps"))
            (kill-matching-lines "^  " (point-min) (point-max))
            (replace-regexp-in-region "^\* \\([a-zA-Z_]+\\) \\([0-9\.]+\\) .+$"
                                      "https://hexdocs.pm/\\1/\\2"
                                      (point-min)
                                      (point-max))
            (completing-read "select the dependency: "
                             (split-string (string-trim (buffer-string)) "\n")))))
    (if force-external
        (browse-url url)
      (if (featurep 'xwidget)
          (xwidget-webkit-browse-url url t)
        (browse-url url)))))

;;;###autoload
(defun vs/elixir-format (&optional project)
  "Format the current elixir file. Format the whole PROJECT if flag is provided."
  (interactive "P")
  (unless (project-current)
    (user-error "Not in a project"))
  (let* ((default-directory (project-root (project-current)))
         (mix (executable-find "mix"))
         (file-name (buffer-file-name))
         (buffer-name "*mix format*")
         (args (append
                `("elixir-format" ,buffer-name ,mix "format")
                (if project '() `(,file-name)))))
    (apply #'start-process args)
    (set-process-sentinel
     (get-buffer-process buffer-name)
     (lambda (process _event)
       (when (> (process-exit-status process) 0 )
         (pop-to-buffer buffer-name))))))

(provide 'vs-elixir)

;;; vs-elixir.el ends here
