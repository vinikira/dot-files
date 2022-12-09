;;; vs-lib.el --- Personal functions -*- lexical-binding: t -*-

;; Author: Vinícius Simões
;; Maintainer: Vinícius Simões
;; Version: version
;; Package-Requires: (dependencies)
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

(require 'cl-macs)
(require 'eieio)
(require 'json)
(require 'nxml-mode)
(require 'project)
(require 'vc-git)

;;;###autoload
(defun vs/split-window-below-and-switch (&optional size)
  "Split the window horizontally, then switch to the new pane.
SIZE is the window size."
  (interactive)
  (call-interactively 'split-window-below t (vector size))
  (other-window 1))

;;;###autoload
(defun vs/split-window-right-and-switch (&optional size)
  "Split the window vertically, then switch to the new pane.
SIZE is the window size."
  (interactive)
  (call-interactively 'split-window-right t (vector size))
  (other-window 1))

;;;###autoload
(defun vs/format-xml-buffer (&optional begin end)
  "Format xml buffer using xmllint, BEGIN region and END region."
  (interactive "r")
  (when (executable-find "xmllint")
    (let ((curr-point (point)))
      (call-shell-region
       (if (region-active-p) begin (point-min))
       (if (region-active-p) end (point-max))
       "xmllint --nowarning --format -"
       t
       (current-buffer))
      (goto-char curr-point))))

;;;###autoload
(defun sudo-edit (&optional arg)
  "Edit file with sudo permission.  ARG."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;;###autoload
(defun vs/indent-buffer ()
  "Indent whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun vs/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;;;###autoload
(defun vs/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;;###autoload
(defun vs/duplicate-current-line (&optional n)
  "Duplicate current line, make more than 1 copy given a N argument."
  (interactive "p")
  (save-excursion
    (let ((current-line (thing-at-point 'line)))
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
        (insert "\n"))
      ;; now insert as many time as requested
      (while (> n 0)
        (insert current-line)
        (setq n (- n 1))))))

;;;###autoload
(defun vs/sh-cmd-to-string (cmd)
  "Execute shell CMD and remove unnecessary newline of output."
  (string-trim
   (shell-command-to-string cmd)))

;;;###autoload
(defun vs/scratch-buffer (new-frame)
  "Create a scratch with selected mode.If NEW-FRAME is t, opens it in new frame."
  (interactive "P")
  (let* ((modes (seq-uniq (mapcar 'cdr auto-mode-alist)))
         (selected-mode
          (completing-read "Select mode: " modes)))
    (when new-frame
      (select-frame (make-frame)))
    (switch-to-buffer
     (get-buffer-create (format "*%s-scratch*" selected-mode)))
    (funcall (intern selected-mode))))

;;;###autoload
(defun vs/verb-graphql (rs)
  "Transform verb RS to graphql request."
  (let* ((before-body (oref rs body))
         (splited-body (split-string before-body "\n\n"))
         (query (nth 0 splited-body))
         (variables (nth 1 splited-body))
         (json-object-type 'alist)
         (parsed-variables (if variables (json-parse-string variables) '()))
         (new-body (json-encode `((query . ,query) (variables . ,parsed-variables)))))
    (oset rs body new-body)
    rs))

;;;###autoload
(defun vs/verb-remove-body-newlines (rs)
  "Remove body newlines from RS."
  (oset rs body (replace-regexp-in-string "\n" "" (oref rs body)))
  rs)

;;;###autoload
(defun vs/garbage-collect ()
  "Run `garbage-collect' and print stats about memory usage."
  (interactive)
  (message (cl-loop for (type size used free) in (garbage-collect)
                    for used = (* used size)
                    for free = (* (or free 0) size)
                    for total = (file-size-human-readable (+ used free))
                    for used = (file-size-human-readable used)
                    for free = (file-size-human-readable free)
                    concat (format "%s: %s + %s = %s\n" type used free total))))

;;;###autoload
(defun vs/nxml-where (&optional copy)
  "Display the hierarchy of XML elements the point is on as a path.
If COPY is non-nil, copy to the clipboard."
  (interactive "p")
  (let ((path nil)
        (formated-path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element) ; always returns nil
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (setq formated-path (format "/%s" (mapconcat 'identity path "/")))
        (if (called-interactively-p t)
            (message formated-path)
          (princ formated-path))
        (when (and copy formated-path)
          (kill-new formated-path))))))

;;;###autoload
(defun vs/read-env-file (file-path clean)
  "Read dot env file from FILE-PATH and set envs in Emacs session.
If CLEAN is provided, all variables listed on file will be
cleared."
  (interactive (list (read-file-name "Select the env file: "
                                     nil
                                     ".env")
                     (y-or-n-p "Clean variables?")))
  (let* ((file-contents (with-temp-buffer
                          (insert-file-contents file-path)
                          (replace-regexp-in-string
                           "export \\|\"\\|^\d*\n\\|^\#.+?\n" ""
                           (buffer-string))))
         (envs (mapcar (lambda (line)
                         (split-string line "="))
                       (split-string file-contents))))
    (dolist (env-pair envs)
      (setenv (car env-pair) (if clean
                                 ""
                               (cadr env-pair))))))

;;;###autoload
(defun vs/update-git-repos (directory branch)
  "Update git repos from DIRECTORY for given BRANCH."
  (interactive (list (read-directory-name "Select the directory: ")
                     (completing-read "Select the principal branch: "
                                      '("master" "main"))))
  (cl-loop for repo in (directory-files directory)
           for full-path = (format "%s%s" directory repo)
           when (and (file-directory-p full-path)
                     (not (or (string= "." repo) (string= repo ".."))))
           do (let* ((full-path (format "%s%s" directory repo))
                     (default-directory full-path))
                (message "Updating repo: %s..." repo)
                (vc-git-retrieve-tag full-path branch t)
                (vc-git-pull nil)
                (message "Done!"))))

;;;###autoload
(defun vs/generate-uid ()
  "Generate an UID and insert at ponint."
  (interactive)
  (unless (executable-find "uuidgen")
    (error "Could not find uuidgen executable"))
  (let* ((uid (shell-command-to-string "uuidgen"))
         (uid-downcased (downcase uid))
         (uid-sanatized (replace-regexp-in-string "\n" "" uid-downcased)))
    (insert uid-sanatized)))

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
              (project-name (vs/project-name))
              (buffer-name (format "*vterm %s*" project-name)))
    (unless (buffer-live-p (get-buffer buffer-name))
      (unless (require 'vterm nil 'noerror)
        (error "Package 'vterm' is not available"))
      (when (fboundp 'vterm)
        (vterm buffer-name)))
    (pop-to-buffer-same-window buffer-name)))

;;;###autoload
(defun vs/kill-ring-unfilled (begin end)
  "Copy the contents of the region BEGIN and END, replace new lines and kill it."
  (interactive "r")
  (let ((text (buffer-substring begin end)))
    (with-temp-buffer
      (insert text)
      (vs/unfill-paragraph)
      (kill-new (buffer-substring (point-min) (point-max))))))

;;;###autoload
(defun vs/iso8601-now ()
  "Insert an ISO8601 date time from now."
  (interactive)
  (insert (format-time-string "%FT%T.%3NZ")))

;;;###autoload
(defun vs/close-project-tab ()
  "Closes the current project tab."
  (interactive)
  (unless (project-current)
    (user-error "The current tab has no open projects"))
  (project-kill-buffers)
  (when (> (length (tab-bar-tabs)) 1)
    (tab-bar-close-tab)))

;;;###autoload
(defun vs/open-with-xwidget-webkit ()
  "Renders HTML buffer with xwe xwidget-webkit."
  (interactive)
  (unless (featurep 'xwidget)
    (user-error "Missing XWidget feature"))
  (if (buffer-file-name)
      (xwidget-webkit-browse-url (format "file://%s" (buffer-file-name)) t)
    (let* ((file-name (format "%s.html" (buffer-hash)))
           (fpath (concat (temporary-file-directory) file-name))
           (buffer (current-buffer)))
      (with-temp-file fpath
        (insert-buffer-substring buffer))
      (xwidget-webkit-browse-url (format "file://%s" fpath) t))))

;;;###autoload
(defun vs/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph/REGION and make it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;;;###autoload
(defun vs/minify (&optional begin end)
  "Minify text from BEGIN to END."
  (interactive "r")
  (save-excursion
    (replace-regexp-in-region "^\\([[:blank:]]+\\)" "" begin end)
    (while (search-forward "\n" end t)
      (replace-match " "))))

(provide 'vs-lib)

;;; vs-lib.el ends here
