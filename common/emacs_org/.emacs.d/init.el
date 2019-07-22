(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
    tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
	       (expand-file-name (concat user-emacs-directory "init.org")))
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)
