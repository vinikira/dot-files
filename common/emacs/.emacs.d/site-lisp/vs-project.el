;;; vs-project.el --- Project helpers  -*- lexical-binding: t -*-

;; Author: Vinícius Simões
;; Maintainer: Vinícius Simões
;; Version: 0.0.1
;; Package-Requires: (project)
;; Homepage: homepage
;; Keywords: Project


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

(require 'project)

;;;###autoload
(defun vs/project-name ()
  "Return the project name for current project."
  (when-let ((project (project-current))
             (dir (project-root project)) )
    (if (string-match "/\\([^/]+\\)/\\'" dir)
        (match-string 1 dir)
      dir)))

;;;###autoload
(defun vs/vterm-in-project ()
  "Invoke `vterm' in the project's root.
Switch to the project specific term buffer if it already exists."
  (interactive)
  (unless (project-current)
    (error "File/buffer doesn't make part of an project"))
  (when-let* ((project (project-current))
              (default-directory (expand-file-name (project-root project)))
              (buffer-name (project-prefixed-buffer-name "vterm")))
    (unless (buffer-live-p (get-buffer buffer-name))
      (unless (require 'vterm nil 'noerror)
        (error "Package 'vterm' is not available"))
      (when (fboundp 'vterm)
        (vterm buffer-name)))
    (pop-to-buffer-same-window buffer-name)))

;;;###autoload
(defun vs/project-runbook ()
  "Open or create the _runbook.org file in the current project."
  (interactive)
  (vs/--project-open-file "_runbook.org"))

;;;###autoload
(defun vs/project-api-file ()
  "Open or create the _api.org file in the current project."
  (interactive)
  (vs/--project-open-file "_api.org"))

;;;###autoload
(defun vs/project-db-file ()
  "Open or create the _db.sql file in the current project."
  (interactive)
  (vs/--project-open-file "_db.sql"))

;;;###autoload
(defun vs/project-dir-locals ()
  "Open or create the .dir-locals.el file in the current project."
  (interactive)
  (vs/--project-open-file ".dir-locals.el"))

(defun vs/--project-open-file (filename)
  "Open or create the FILENAME file in the current project."
  (unless (project-current)
    (error "File/buffer doesn't make part of an project"))
  (when-let* ((project (project-current))
              (default-directory (expand-file-name (project-root project))))
    (find-file filename)))

;;;###autoload
(defun vs/project-save-project-buffers ()
  "Save all project buffers."
  (interactive)
  (let* ((project (project-current))
         (project-name (project-name project))
         (modified-buffers (cl-remove-if-not (lambda (buf)
                                               (and (buffer-file-name buf)
                                                    (buffer-modified-p buf)))
                                             (project-buffers project))))
    (if (null modified-buffers)
        (message "[%s] No buffers need saving" project-name)
      (dolist (buf modified-buffers)
        (with-current-buffer buf
          (save-buffer)))
      (message "[%s] Saved %d buffers" project-name (length modified-buffers)))))

(provide 'vs-project)

;;; vs-project.el ends here
