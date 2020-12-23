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
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun vs/indent-buffer ()
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
  "Execute shell CMD and remove newline of output."
  (shell-command-to-string
   (concat "printf %s \"$(" cmd ")\"")))

(provide 'base-functions)
;;; base-functions.el ends here
