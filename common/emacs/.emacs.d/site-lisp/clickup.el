;;; clickup.el --- Org mode clickup integration -*- lexical-binding: t -*-

;; Author: Vinícius Simões
;; Maintainer: Vinícius Simões
;; Version: version
;; Package-Requires: (password-store)
;; Homepage: homepage
;; Keywords: keywords


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

(require 'subr-x)
(require 'org)

;; Clickup little integration
(defcustom clickup-org-token-entry nil
  "Entry name on the password store to get the clickup token."
  :type 'string
  :group 'org
  :safe 'stringp)

;;;###autoload
(defun clickup-org-fill-task ()
  "Fill Clickup task."
  (interactive)
  (when-let* ((pt (point))
              (task-id (and (org-at-heading-p)
                            (car (split-string
                                  (nth 4 (org-heading-components))
                                  " ")))))
    (let-alist (clickup-org-get-task task-id)
      (let ((headline (format "%s - %s" .id .name)))
        (message "Updating %s" headline)
        (org-edit-headline headline)
        (cl-loop
         for (property value)
         on (list
             "ClickupAssignee" (let-alist (nth 0 .assignees)
                                 .username)
             "ClickupCreated" .date_created
             "ClickupTaskId" .id
             "ClickupCreator" .creator.username
             "ClickupStatus" .status.status
             "ClickupDescription" .description)
         by 'cddr
         do (if (string= property "ClickupDescription")
                (progn
                  (org-end-of-subtree)
                  (org-insert-heading)
                  (org-demote)
                  (insert "Description")
                  (newline)
                  (insert value))
              (org-set-property property value)))))))

(autoload 'password-store-get "password-store")

;;;###autoload
(defun clickup-org-get-task (task-id)
  "Get Clickup task associated to TASK-ID."
  (unless (executable-find "curl")
    (error "CURL is missing"))
  (let* ((url (format "https://api.clickup.com/api/v2/task/%s/" task-id))
         (auth-header (format "Authorization: %s"
                              (password-store-get clickup-org-token-entry)))
         (command (format "curl -s -H \"%s\" '%s'" auth-header url)))
    (json-parse-string (shell-command-to-string command)
                       :object-type 'alist
                       :array-type 'list
                       :null-object nil)))

(provide 'clickup)

;;; clickup.el ends here
