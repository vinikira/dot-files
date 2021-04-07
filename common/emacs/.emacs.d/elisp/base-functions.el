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
  "Execute shell CMD and remove newline of output."
  (shell-command-to-string
   (concat "printf %s \"$(" cmd ")\"")))

(defun vs/json-to-etf (&optional begin end)
  "Transform JSON to Elixir Term Format.  Use BEGIN and END as region."
  (interactive "r")
  (save-excursion
    (let ((min (if (region-active-p) begin (point-min))))
      (goto-char min)
      (while (re-search-forward ":" end t)
	(replace-match " =>"))
      (goto-char min)
      (while (re-search-forward "{" end t)
	(replace-match "%{")))))

(defun vs/etf-to-json (&optional begin end)
  "Transform Elixir Term Format to JSON.  Use BEGIN and END as region."
  (interactive "r")
  (save-excursion
    (let ((min (if (region-active-p) begin (point-min))))
      (goto-char min)
      (while (re-search-forward " =>" end t)
	(replace-match ":"))
      (goto-char min)
      (while (re-search-forward "%{" end t)
	(replace-match "{")))))

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

(provide 'base-functions)

;;; base-functions.el ends here
