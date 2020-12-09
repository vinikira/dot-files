;;; layer-org.el --- Org mode Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Org mode latest version.
;; =============================================================================
(straight-use-package 'org-plus-contrib)

;; Defining where the Org files will be stored.
(defconst vs/org-directory
  (if (file-directory-p "~/Sync/org/") "~/Sync/org/" "~/"))

;; My Org capture templates.
(defconst vs/org-capture-templates
  '(("t" "TODO menu")
    ("tp" "TODO personal" entry (file+headline "personal.org" "Tasks")
     "* TODO %?\nSCHEDULED: ^%^t\n%u\n%a\n")
    ("tw" "TODO work" entry (file+headline "work.org" "Tasks")
     "* TODO %?\nSCHEDULED: %^t\n%u\n%a\n")
    ("m" "Meetings menu")
    ("mp" "Meeting personal" entry (file+headline "personal.org" "Meetings")
     "* MEETING with %? :MEETING:\nSCHEDULED: %^t")
    ("mw" "Meeting work" entry (file+headline "work.org" "Meetings")
     "* MEETING with %? :MEETING:\nSCHEDULED: %^t")
    ("n" "Note" entry (file org-default-notes-file)
     "* %? \n%t")))

;; My Org structure templates.
(defconst vs/org-structure-template-alist
  '(("n" . "notes")
    ("a" . "export ascii")
    ("c" . "center")
    ("C" . "comment")
    ("e" . "example")
    ("E" . "export")
    ("h" . "export html")
    ("l" . "export latex")
    ("q" . "quote")
    ("s" . "src")
    ("v" . "verse")))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
(add-hook 'org-mode-hook 'toggle-word-wrap)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook '(lambda () (display-line-numbers-mode -1)))
(add-hook 'org-mode-hook 'auto-fill-mode)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(customize-set-variable 'org-directory vs/org-directory)
(customize-set-variable 'org-default-notes-file (concat org-directory "notes.org"))
(customize-set-variable 'org-agenda-files (list (concat org-directory "work.org")
						(concat org-directory "personal.org")))
(customize-set-variable 'org-confirm-babel-evaluate nil)
(customize-set-variable 'org-src-fontify-natively t)
(customize-set-variable 'org-log-done 'time)
(customize-set-variable 'org-babel-sh-command "bash")
(customize-set-variable 'org-capture-templates vs/org-capture-templates)
(customize-set-variable 'org-structure-template-alist vs/org-structure-template-alist)
(customize-set-variable 'org-use-speed-commands t)
(customize-set-variable 'org-refile-use-outline-path t)
(customize-set-variable 'org-outline-path-complete-in-steps nil)
(customize-set-variable 'org-refile-targets '((org-agenda-files :maxlevel . 9)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (js . t)
   (shell . t)
   (plantuml . t)
   (sql . t)
   (elixir . t)
   (ruby . t)
   (dot . t)
   (latex . t)))
;; =============================================================================

;; Org Bullets
;; =============================================================================
(straight-use-package 'org-bullets)

(add-hook 'org-mode-hook (lambda ()
			   (require 'org-bullets)
			   (org-bullets-mode 1)))

(customize-set-variable 'org-hide-leading-stars t)
;; =============================================================================

;; Org Download
;; =============================================================================
(straight-use-package 'org-download)

(add-hook 'dired-mode-hook 'org-download-enable)
;; =============================================================================

;; Org + Reveal.js
;; =============================================================================
(straight-use-package 'org-re-reveal)

(customize-set-variable 'org-re-reveal-root
			"https://cdn.jsdelivr.net/reveal.js/latest")
(customize-set-variable 'org-reveal-mathjax t)
;; =============================================================================

;; Org Verb
;; =============================================================================
(straight-use-package 'verb)

(with-eval-after-load 'org
  (require 'verb)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
;; =============================================================================

;; Org Babel Async
;; =============================================================================
;; Turn code evaluation async.
(straight-use-package 'ob-async)

(with-eval-after-load 'ob
  (require 'ob-async)
  (customize-set-variable 'ob-async-no-async-languages-alist '("ipython")))

;; =============================================================================

;; Org Pandoc
;; =============================================================================
(straight-use-package 'ox-pandoc)

(with-eval-after-load 'ox
  (require 'ox-pandoc))
;; =============================================================================

;; Org Notify
;; =============================================================================
(with-eval-after-load 'org
  (require 'org-notify)
  (org-notify-start 60)

  (org-notify-add
   'default
   '(:time "10m" :period "2m" :duration 25 :actions -notify/window)
   '(:time "1h" :period "15m" :duration 25 :actions -notify/window)
   '(:time "2h" :period "30m" :duration 25 :actions -notify/window)))
;; =============================================================================

;; Org Jira
;; =============================================================================
(straight-use-package
 '(ox-jira :type git :host github :repo "stig/ox-jira.el" :branch "trunk"))
;; =============================================================================

;; Latex
;; =============================================================================
;; Abntex2 class
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("abntex2"
		 "\\documentclass{abntex2}
                    [NO-DEFAULT-PACKAGES]
                    [EXTRA]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")
		 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
		 ("\\maketitle" . "\\imprimircapa"))))

;; Source code highlight with Minted package.
(customize-set-variable 'org-latex-listings 'minted)
(customize-set-variable 'org-latex-packages-alist '(("" "minted")))
(customize-set-variable
 'org-latex-pdf-process
 '("latexmk -shell-escape -pdf -interaction=nonstopmode -file-line-error %f"))
;; =============================================================================

(provide 'layer-org)

;;; layer-org.el ends here
