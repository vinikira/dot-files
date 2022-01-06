;;; base-functions.el --- Custom functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun vs/split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (other-window 1))

(defun vs/split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (other-window 1))

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

(defun sudo-edit (&optional arg)
  "Edit file with sudo permission.  ARG."
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun vs/indent-buffer ()
  "Indent whole buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun vs/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun vs/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

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
        (decf n)))))

(defun vs/sh-cmd-to-string (cmd)
  "Execute shell CMD and remove unnecessary newline of output."
  (string-trim
   (shell-command-to-string cmd)))

(defun vs/scratch-buffer (new-frame)
  "Create a scratch with selected mode.If NEW-FRAME is t, opens it in new frame."
  (interactive "P")
  (let* ((modes (seq-uniq (mapcar #'cdr auto-mode-alist)))
         (selected-mode
          (completing-read "Select mode: " modes)))
    (when new-frame
      (select-frame (make-frame)))
    (switch-to-buffer
     (get-buffer-create (format "*%s-scratch*" selected-mode)))
    (funcall (intern selected-mode))))

(declare-function oref "eieio")
(declare-function oset "eieio")
(declare-function json-encode "json")

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

(defun vs/verb-remove-body-newlines (rs)
  "Remove body newlines from RS."
  (oset rs body (replace-regexp-in-string "\n" "" (oref rs body)))
  rs)

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

(defun vs/read-env-file (clean)
  "Read dot env file and set envs in Emacs session.
If CLEAN is provided, all variables listed on file will be
cleared."
  (interactive (list (y-or-n-p "Clean variables?")))
  (let* ((file-path (format "%s.env" (or (vc-root-dir) "./")))
         (file-contents (with-temp-buffer
                          (insert-file-contents file-path)
                          (buffer-string)))
         (envs (mapcar (lambda (line)
                         (split-string line "="))
                       (split-string file-contents))))
    (dolist (env-pair envs)
      (setenv (car env-pair) (if clean
                                 ""
                               (cadr env-pair))))))

(defun vs/update-git-repos (directory branch)
  "Update git repos from DIRECTORY for given BRANCH."
  (interactive (list (read-directory-name "Select the directory: ")
                     (completing-read "Select the principal branch: "
                                      '("master" "main"))))
  (dolist (repo (directory-files directory))
    (when (not (or (string= "." repo) (string= repo "..")))
      (let* ((full-path (format "%s%s" directory repo))
             (default-directory full-path))
        (message "Updating repo: %s..." repo)
        (vc-git-retrieve-tag full-path branch t)
        (vc-git-pull nil)
        (message "Done!")))))

(provide 'base-functions)

;;; base-functions.el ends here
