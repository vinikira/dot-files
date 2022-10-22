;;; tangle.el --- summary -*- lexical-binding: t -*-
;;
;; Author: Vinícius Simões <vinicius.simoes95@gmail.com>
;; Maintainer: Vinícius Simões
;; Homepage:
;; Version: 0.0.1 Alpha
;; Keywords:
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2022 Vinícius Simões
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;;; Code:

;; tangle the source code blocks require org
(require 'org)

(defvar init-org-file
  (expand-file-name "emacs-config.org" user-emacs-directory)
  "The init org file - full path.")

(defvar site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory)
  "The site-lisp directory - full path.")

(defvar site-lisp-autoload-file
  (expand-file-name "site-lisp-loaddefs.el" site-lisp-directory)
  "The site-lisp autoload file - full path.")

(defun tangle-config ()
  "Tangle the configuration to their files."
  (org-babel-tangle-file init-org-file))

(defun generate-autoload-site-lisp ()
  "Gerate the autoload file for site-lisp directory."
  (make-directory-autoloads site-lisp-directory site-lisp-autoload-file))

(provide 'tangle)

;;; tangle.el ends here
