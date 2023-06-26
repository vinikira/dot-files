;;; clickup.el --- Emacs ClickUp integration -*- lexical-binding: t -*-

;; Author: Vinícius Simões
;; Maintainer: Vinícius Simões
;; Version: version
;; Package-Requires: ((emacs 26.1))
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
(require 'json)
(require 'url)
(require 'widget)

(defgroup clickup nil
  "Group for ClickUp.el configurations."
  :group 'clickup
  :prefix "clickup-")

(defcustom clickup-token nil
  "API token to access ClickUp services."
  :type 'string
  :group 'clickup
  :safe 'stringp)

(defcustom clickup-api-host "https://api.clickup.com"
  "ClickUp API host."
  :type 'string
  :group 'clickup
  :safe 'stringp)

(defvar clickup--loading nil
  "Define is an HTTP call is happening.")

(defvar clickup--last-response nil
  "Store the last response from ClickUp API")

(defvar clickup--view-tasks nil
  "Store all tasks for a View")

(defvar clickup--team-id nil
  "Store the selected team id.")

(defvar clickup--space-id nil
  "Store the selected space id.")

(defvar clickup--list-id nil
  "Store the selected list id.")

(defvar clickup--view-id nil
  "Store the view id.")

;;; EMACS lisp client

(defun clickup-get-task (task-id)
  "Get ClickUp task associated to TASK-ID."
  (clickup--request (format "/api/v2/task/%s/?include_subtasks=true" task-id)
                    nil
                    clickup-token)
  (clickup--wait-next-response)
  (let-alist clickup--last-response
    `((id . ,.id)
      (name . ,.name)
      (description . ,.description)
      (status . ,.status.status)
      (status-color . ,.status.color)
      (url . ,.url)
      (creator . ,.creator.username)
      (assignees . ,(cl-loop for ass in .assignees
                             collect (alist-get 'username ass)))
      (date-created . ,.date_created)
      (date-updated . ,.date_updated)
      (date-done . ,.date_done)
      (date-closed . ,.date_closed)
      (team-id . ,.team_id)
      (parent . ,.parent)
      (list . ,.list.id)
      (project . ,.project.id)
      (subtasks . ,(cl-loop for subtask in .subtasks
                            collect (clickup--map-task-search-result subtask))))))

(defun clickup--map-task-search-result (task)
  "Maps a TASK search result to the internal form."
  (let-alist task
    `((id . ,.id)
      (name . ,.name)
      (status . ,.status.status)
      (url . ,.url)
      (creator . ,.creator.name)
      (assignees . ,(cl-loop for ass in .assignees
                             collect (alist-get 'username ass)))
      (date-created . ,.date_created)
      (date-closed . ,.date_closed))))

(defun clickup-get-lists (folder-id)
  "Get ClickUp lists associated to FOLDER-ID."
  (clickup--request (format "/api/v2/folder/%s/list?archived=false" folder-id)
                    nil
                    clickup-token))

(defun clickup-get-spaces (team-id)
  "Get ClickUp spaces associated to TEAM-ID."
  (clickup--request (format "/api/v2/team/%s/space?archived=false" team-id)
                    nil
                    clickup-token)
  (clickup--wait-next-response)
  (let-alist clickup--last-response
    (cl-loop for space in .spaces
             collect (let-alist space
                       `((id . ,.id)
                         (name . ,.name))))))

(defun clickup-get-folders (space-id)
  "Get ClickUp folders associated to SPACE-ID."
  (clickup--request (format "/api/v2/space/%s/folder?archived=false" space-id)
                    nil
                    clickup-token)
  (clickup--wait-next-response)
  (let-alist clickup--last-response
    (cl-loop for space in .spaces
             collect (let-alist space
                       `((id . ,.id)
                         (name . ,.name))))))

(defun clickup-get-folderless-lists (space-id)
  "Get ClickUp folderless lists associated to SPACE-ID."
  (clickup--request (format "/api/v2/space/%s/list?archived=false" space-id)
                    nil
                    clickup-token)
  (clickup--wait-next-response)
  (let-alist clickup--last-response
    (cl-loop for list in .lists
             collect (let-alist list
                       `((id . ,.id)
                         (name . ,.name))))))

(defun clickup-get-authorized-teams ()
  "Get ClickUp authorized teams."
  (clickup--request "/api/v2/team"
                    nil
                    clickup-token)
  (clickup--wait-next-response)
  (let-alist clickup--last-response
    (cl-loop for team in .teams
             collect (let-alist team
                       `((id . ,.id)
                         (name . ,.name))))))

;; TODO
(defun clickup-list-views (list-id)
  "Get ClickUp view associated to LIST-ID."
  (clickup--request (format "/api/v2/list/%s" list-id)
                    nil
                    clickup-token))

(defun clickup-get-view (view-id)
  "Get ClickUp view associated to VIEW-ID."
  (clickup--request (format "/api/v2/view/%s/" view-id)
                    nil
                    clickup-token))

(defun clickup-get-folder-views (folder-id)
  "Get ClickUp views associated to FOLDER-ID."
  (clickup--request (format "/api/v2/folder/%s/view" folder-id)
                    nil
                    clickup-token))

(defun clickup-get-space-views (space-id)
  "Get ClickUp views associated to SPACE-ID."
  (clickup--request (format "/api/v2/space/%s/view" space-id)
                    nil
                    clickup-token)
  (clickup--wait-next-response)
  (let-alist clickup--last-response
    (cl-loop for view in .views
             collect (let-alist view
                       `((id . ,.id)
                         (name . ,.name)
                         (parent . ,.parent.id))))))

(defun clickup-get-list-views (list-id)
  "Get ClickUp views associated to LIST-ID."
  (clickup--request (format "/api/v2/list/%s/view" list-id)
                    nil
                    clickup-token)
  (clickup--wait-next-response)
  (let-alist clickup--last-response
    (cl-loop for view in .views
             collect (let-alist view
                       `((id . ,.id)
                         (name . ,.name)
                         (parent . ,.parent.id))))))

(defun clickup-get-view-tasks (view-id)
  "Get ClickUp view associated to VIEW-ID."
  (let ((page 0)
        (should-fetch? t))
    (while should-fetch?
      (message "calling %s" (format "/api/v2/view/%s/task?page=%s" view-id page))
      (clickup--request (format "/api/v2/view/%s/task?page=%s" view-id page)
                        nil
                        clickup-token)
      (clickup--wait-next-response)
      (setq should-fetch? (not (let-alist clickup--last-response .last_page))
            page (cl-incf page)
            clickup--view-tasks
            (append clickup--view-tasks
                    (let-alist clickup--last-response
                      (cl-loop
                       for task in .tasks
                       collect (clickup--map-task-search-result task))))))))

;;; API access

(defun clickup--request(path &optional body token)
  "Perform HTTP requests to Click UP API using the TOKEN.
Sends the BODY to the PATH."
  (let ((url-request-method "GET")
        (url-address (format "%s%s" clickup-api-host path))
        (url-request-extra-headers `(("Authorization" . ,token))))
    (setq clickup--loading t
          clickup--last-response nil)
    (url-retrieve url-address #'clickup--request-callback)))

(defun clickup--request-callback (status)
  "ClickUp request callback. STATUS."
  (when (alist-get :error status)
    (user-error "ClickUp error: %s"
                (alist-get 'error (alist-get :error status))))
  (goto-char (point-min))
  (search-forward "\n\n")
  (let ((json-object-type 'alist)
        (json-array-type 'list))
    (setq clickup--last-response (json-read)
          clickup--loading nil)))

(defun clickup--wait-next-response ()
  "Wait until next async response."
  (while clickup--loading
    (sleep-for 0.1)))

;;; UI

;;;###autoload
(defun clickup-root ()
  "Opens ClickUp root page."
  (interactive)
  (switch-to-buffer "*ClickUp for Emacs*")
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (make-local-variable 'clickup--team-id)
  (make-local-variable 'clickup--space-id)
  (make-local-variable 'clickup--list-id)
  (make-local-variable 'clickup--view-id)
  (widget-insert (propertize "ClickUp for Emacs\n\n" 'face '(:height 1.5 :weight bold)))
  (let ((teams (cl-loop for team in (clickup-get-authorized-teams)
                        collect (let-alist team
                                  `(item :tag ,.name :value ,.id)))))
    (widget-create 'menu-choice
                   :tag "Team"
                   :value clickup--team-id
                   :help-echo "Select the team to view the workspace"
                   :notify #'clickup--team-menu-choice-callback
                   :args teams)

    (use-local-map widget-keymap)
    (widget-setup)
    (goto-char (point-min))))

(defun clickup--team-menu-choice-callback (widget &rest _ignore)
  "Callback for the team menu choice WIDGET."
  (setq clickup--team-id (widget-value widget)
        clickup--space-id nil
        clickup--list-id nil
        clickup--view-id nil)
  (goto-char (point-max))
  (widget-create 'menu-choice
                 :tag "Space"
                 :value clickup--space-id
                 :help-echo "Select the space to view the list"
                 :args (cl-loop for space in (clickup-get-spaces clickup--team-id)
                                collect (let-alist space
                                          `(item :tag ,.name :value ,.id)))
                 :notify #'clickup--space-menu-choice-callback)
  (widget-setup))

(defun clickup--space-menu-choice-callback (widget &rest _ignore)
  "Callback for the space menu choice WIDGET."
  (setq clickup--space-id (widget-value widget)
        clickup--list-id nil
        clickup--view-id nil)
  (goto-char (point-max))
  (widget-create 'menu-choice
                 :tag "List"
                 :value clickup--list-id
                 :help-echo "Select the list to view the views"
                 :args
                 (cl-loop for folder in (clickup-get-folderless-lists clickup--space-id)
                          collect (let-alist folder
                                    `(item :tag ,.name :value ,.id)))
                 :notify #'clickup--list-menu-choice-callback)
  (widget-setup))

(defun clickup--list-menu-choice-callback (widget &rest _ignore)
  "Callback for the list menu choice WIDGET."
  (setq clickup--list-id (widget-value widget)
        clickup--view-id nil)
  (goto-char (point-max))
  (widget-create 'menu-choice
                 :tag "Views"
                 :value clickup--view-id
                 :help-echo "Select the view to view the tasks"
                 :args
                 (cl-loop for list  in (clickup-get-list-views clickup--list-id)
                          collect (let-alist list
                                    `(item :tag ,.name :value ,.id)))
                 :notify #'clickup--view-menu-choice-callback)
  (widget-setup))

(defun clickup--view-menu-choice-callback (widget &rest _ignore)
  "Callback for the view menu choice WIDGET."
  (setq clickup--view-id (widget-value widget))
  (clickup--load-view-tasks-widgets))

(defun clickup--load-view-tasks-widgets ()
  "Load tasks widgets from `clickup--view-id'."
  (when (null clickup--view-id)
    (user-error "No view selected"))
  (make-local-variable 'clickup--view-tasks)
  (clickup-get-view-tasks clickup--view-id)
  (goto-char (point-max))
  (widget-insert (propertize "\nTASKS \n\n" 'face '(:height 1.2)))
  (thread-last
    clickup--view-tasks
    (seq-group-by (lambda (task)
                    (alist-get 'status task)))
    (mapc (lambda (status-task)
            (widget-insert (propertize
                            (upcase (format "%s\n" (car status-task)))
                            'face '(:height 1.2)))
            (cl-loop for task in (cdr status-task)
                     do (clickup--task-search-result-widget task))
            (widget-insert "\n\n"))))
  (widget-setup))

(defun clickup--task-search-result-widget (task)
  "Create a widget for the TASK result search."
  (widget-create 'link
                 :tag (alist-get 'name task)
                 :format (concat "%[Open%] %t\n")
                 :notify
                 (lambda (&rest _ignore)
                   (funcall-interactively
                    #'clickup-task (alist-get 'id task)))
                 (format "%s\n" (alist-get 'name task))))

;;;###autoload
(defun clickup-task (task-id)
  "Show the TASK-ID details."
  (interactive (list (read-string "Enter the task id: ")))
  (message "loading task id: %s" task-id)
  (let-alist (clickup-get-task task-id)
    (switch-to-buffer (format "*ClickUp Task %s*" .id))
    (kill-all-local-variables)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (widget-insert (propertize (format "[%s] " .status)
                               'face
                               `(:height 1.2 :weight bold :foreground ,.status-color)))
    (widget-insert (propertize (format "%s\n\n" .name) 'face '(:height 1.2)))
    (widget-insert "DESCRIPTION\n\n")
    (widget-insert (format "%s\n\n" (if (string-empty-p .description)
                                        "No description."
                                      .description)))
    (widget-insert (format (concat (propertize "Id:" 'face '(:weight bold)) " %s\n")
                           .id))
    (widget-insert (format (concat (propertize "Date Created:" 'face '(:weight bold)) " %s\n")
                           (format-time-string "%Y-%m-%d %H:%M:%S"
                                               (seconds-to-time (/ (string-to-number .date-created) 1000)))))
    (widget-insert (format (concat (propertize "Date Updated:" 'face '(:weight bold)) " %s\n")
                           (format-time-string "%Y-%m-%d %H:%M:%S"
                                               (seconds-to-time (/ (string-to-number .date-updated) 1000)))))
    (widget-insert (format (concat (propertize "Creator:" 'face '(:weight bold)) " %s\n") .creator))
    (when .subtasks
      (widget-insert "\nSUBTASKS\n\n")
      (cl-loop for subtask in .subtasks
               do (clickup--task-search-result-widget subtask)))
    (widget-insert "\n")
    (widget-insert "\nACTIONS \n\n")
    (widget-create 'link
                   :notify
                   (lambda (&rest _ignore)
                     (browse-url .url))
                   "URL"))
  (widget-setup))

(provide 'clickup)

;;; clickup.el ends here
