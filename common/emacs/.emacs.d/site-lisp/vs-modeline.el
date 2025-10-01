;;; vs-modeline.el --- My modeline -*- lexical-binding: t -*-

;; Author: Vinícius Simões
;; Maintainer: Vinícius Simões
;; Version: 0.0.1
;; Package-Requires: ((emacs . "29.1"))
;; Homepage: homepage
;; Keywords: emacs modeline


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

(autoload 'nerd-icons-faicon "nerd-icons")
(autoload 'nerd-icons-devicon "nerd-icons")
(autoload 'nerd-icons-icon-for-buffer "nerd-icons")

(defgroup vc-modeline nil
  "Custom group for vs-modeline."
  :group 'mode-line)

(defvar-local vs-modeline--mode-line-format-default nil
  "Default value for `mode-line-format'.")

(defun vs-modeline--line-column ()
  "Define the mode for lines and columns."
  (concat
   (nerd-icons-faicon "nf-fa-code")
   "  %l:%c"))

(defvar-local vs-modeline-line-column
    '(:eval
      (when (and
             (mode-line-window-selected-p)
             (derived-mode-p 'prog-mode 'text-mode))
        (concat
         (vs-modeline--line-column)
         "  ")))
  "Mode line construct displaying buffer line and column.
Only show for text and prog modes.")

(defun vs-modeline--major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (concat
   (nerd-icons-icon-for-buffer)
   " "
   (capitalize
    (string-replace
     "-mode"
     ""
     (symbol-name major-mode)))))

(defvar-local vs-modeline-major-mode
    '(:eval
      (concat
       (vs-modeline--major-mode-indicator)
       "  "))
  "Mode line construct displaying `major-mode'.")

(defun vs-modeline--buffer-name ()
  "Mode line construct displaying buffer name."
  (let ((file (buffer-file-name)))
    (if (and file (buffer-modified-p))
        (propertized-buffer-identification "* %b  ")
      (propertized-buffer-identification "%b  "))))

(defvar-local vs-modeline-buffer-name
    '(:eval (vs-modeline--buffer-name))
  "Mode line construct displaying the buffer name.")

(defun vs-modeline--vc ()
  "Mode line construct displaying version control information."
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (nerd-icons-devicon "nf-dev-git_branch")
     " "
     (propertize (substring branch 0 15) 'help-echo branch))))

(defvar-local vs-modeline-vc
    '(:eval
      (when (and (bound-and-true-p vc-mode) (mode-line-window-selected-p))
        (concat
         (vs-modeline--vc)
         " ")))
  "Mode line construct displaying version control information.")

(defvar-local vs-modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

(defvar-local vs-modeline-right-space
    '(:eval
      (when (mode-line-window-selected-p)
        (propertize " " 'display '(space . (:align-to (- right 15))))))
  "Mode line construct moving all the widgets after that to the right side.")

(defun vs-modeline--clock-calendar ()
  "Return a propertized string with clock and calendar."
  (concat
   (nerd-icons-faicon "nf-fa-clock_o")
   (propertize (format-time-string " %H:%M")
               'face '(:height 0.9)
               'mouse-face 'mode-line-highlight
               'local-map (make-mode-line-mouse-map 'mouse-1 'world-clock))
   " "
   (nerd-icons-faicon "nf-fa-calendar")
   (propertize (format-time-string "  %b %d")
               'face '(:height 0.9)
               'mouse-face 'mode-line-highlight
               'local-map (make-mode-line-mouse-map 'mouse-1 'calendar))))

(defvar-local vs-modeline-clock-calendar
    '(:eval
      (when (mode-line-window-selected-p)
        (concat
         (vs-modeline--clock-calendar)
         "  ")))
  "Mode line construct displaying clock and calendar.")

(dolist (construct '(vs-modeline-line-column
                     vs-modeline-major-mode
                     vs-modeline-buffer-name
                     vs-modeline-vc
                     vs-modeline-misc-info
                     vs-modeline-right-space
                     vs-modeline-clock-calendar))
  (put construct 'risky-local-variable t))

(defun vs-modeline--enable ()
  "Enable vs-modeline."
  (setq vs-modeline--mode-line-format-default mode-line-format)
  (setq-default mode-line-format
                '("%e  "
                  vs-modeline-major-mode
                  vs-modeline-buffer-name
                  vs-modeline-line-column
                  vs-modeline-vc
                  vs-modeline-misc-info
                  vs-modeline-right-space
                  vs-modeline-clock-calendar)))

(defun vs-modeline--disable ()
  "Disable vs-modeline."
  (setq-default mode-line-format vs-modeline--mode-line-format-default))

;;;###autoload
(define-minor-mode vs-modeline-mode
  "Personal modeline."
  :global t
  :group 'vs-modeline
  (if vs-modeline-mode
      (vs-modeline--enable)
    (vs-modeline--disable)))

(provide 'vs-modeline)

;;; vs-modeline.el ends here
