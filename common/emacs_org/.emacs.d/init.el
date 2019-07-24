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
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(defconst private-dir (expand-file-name "private" user-emacs-directory))
(defconst temp-dir (format "%s/cache" private-dir))

(unless (file-exists-p private-dir)
  (make-directory private-dir :parents))

(unless (file-exists-p temp-dir)
  (make-directory temp-dir :parents))

(setq custom-file (expand-file-name (concat private-dir "/custom.el")))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file)

(defconst vs/chosen-font-name "xos4 Terminus")

(use-package dracula-theme
  :config (load-theme 'dracula t))

(use-package telephone-line
  :config (telephone-line-mode 1))

(setq inhibit-startup-screen t
      inhibit-splash-screen t
      mouse-wheel-follow-mouse t
      scroll-step 1
      scroll-conservatively 101)

(show-paren-mode 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(defconst vs/org-directory (if (file-directory-p "~/Sync/org")
                               "~/Sync/org"
                             "~/"))

(use-package org
  :ensure org-plus-contrib
  :hook ((org-mode . toggle-word-wrap)
         (org-mode . org-indent-mode)
         (org-mode . turn-on-visual-line-mode))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)))

(setq org-directory vs/org-directory
      org-default-notes-file (concat org-directory "/notes.org")
      org-agenda-files '((concat org-directory "/work.org")
                         (concat org-directory "/personal.org"))
      org-confirm-babel-evaluate t
      org-src-fontify-natively t
      org-log-done 'time
      org-babel-sh-command "bash")

(setq org-capture-templates '(("t" "todo" entry (file org-default-notes-file)
                                "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
                               ("m" "Meeting" entry (file org-default-notes-file)
                                "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
                               ("d" "Diary" entry (file+datetree "~/org/diary.org")
                                "* %?\n%U\n" :clock-in t :clock-resume t)
                               ("i" "Idea" entry (file org-default-notes-file)
                                "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
                               ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
                                "** NEXT %? \nDEADLINE: %t")))

(org-babel-do-load-languages
 'org-babel-load-languages
 (org-babel-do-load-languages
  'org-babel-load-languages
  (append org-babel-load-languages
          '((emacs-lisp . t)
            (python . t)
            (restclient . t)
            (js . t)
            (shell . t)
            (plantuml . t)
            (sql . t)
            (ipython . t)))))
