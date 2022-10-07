;;; layer-org.el --- Org mode Layer -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(declare-function straight-use-package "ext:straight")

;; Org mode latest version.
;; =============================================================================
(straight-use-package '(org-contrib :type git
                                    :includes (org)
                                    :host nil
                                    :repo "https://git.sr.ht/~bzg/org-contrib"
                                    :files (:defaults "lisp/*.el")))

;; Defining where the Org files will be stored.
(defconst vs/org-directory
  (if (file-directory-p "~/Sync/org/") "~/Sync/org/" "~/org"))

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
     "* %? \n%t")
    ("j" "Journal Entry" entry (file+datetree "journal.org")
     "* Event: %?\n\n  %i\n\n  From: %a"
     :empty-lines 1)
    ("c" "New Contact" entry (file "contacts.org")
     "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE:
:ALIAS:
:NICKNAME:
:IGNORE:
:ICON:
:NOTE:
:ADDRESS:
:BIRTHDAY:
:END:")))

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

;; Org Agenda Custom Commands
(declare-function org-end-of-subtree "org")
(declare-function org-get-priority "org")
(declare-function org-entry-get "org")

(defun vs/--org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.
PRIORITY may be one of the characters ?A, ?B, or ?C."
  (when (boundp 'org-lowest-priority)
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil))))

(defun vs/--org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(customize-set-variable
 'org-agenda-custom-commands
 '(("c" "Complete agenda view"
    ((tags "PRIORITY=\"A\""
           ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
            (org-agenda-overriding-header "High-priority unfinished tasks:")))
     (agenda "")
     (alltodo ""
              ((org-agenda-skip-function
                '(or (vs/--org-skip-subtree-if-habit)
                     (vs/--org-skip-subtree-if-priority ?A)
                     (org-agenda-skip-if nil '(scheduled deadline))))
               (org-agenda-overriding-header "ALL normal priority tasks:")))))))

(declare-function org-display-inline-images "org")
(declare-function org-indent-mode "org-indent")
(declare-function org-store-link "ol")

(add-hook 'org-babel-after-execute-hook #'org-display-inline-images 'append)
(add-hook 'org-mode-hook #'toggle-word-wrap)
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'turn-on-visual-line-mode)
(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'org-mode-hook #'auto-fill-mode)

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(customize-set-variable 'org-directory vs/org-directory)

(when (boundp 'org-directory)
  (customize-set-variable 'org-default-notes-file (concat org-directory "notes.org"))
  (customize-set-variable 'org-agenda-files (list (concat org-directory "work.org")
                                                  (concat org-directory "personal.org"))))

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
(customize-set-variable 'org-hide-emphasis-markers t)

(with-eval-after-load 'org
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
     (latex . t))))
;; =============================================================================

;; Org Bullets
;; =============================================================================
(straight-use-package 'org-bullets)

(declare-function org-bullets-mode "ext:org-bullets")

(add-hook 'org-mode-hook #'org-bullets-mode)

(customize-set-variable 'org-hide-leading-stars t)
;; =============================================================================

;; Org Download
;; =============================================================================
(straight-use-package 'org-download)

(declare-function org-download-enable "ext:org-download")

(add-hook 'dired-mode-hook #'org-download-enable)
;; =============================================================================

;; Org Present
;; ============================================================================
(straight-use-package 'org-present)

(declare-function org-present-big "ext:org-present")
(declare-function org-present-hide-cursor "ext:org-present")
(declare-function org-present-read-only "ext:org-present")
(declare-function org-present-small "ext:org-present")
(declare-function org-present-show-cursor "ext:org-present")
(declare-function org-present-read-write "ext:org-present")

(declare-function org-remove-inline-images "org")

(add-hook 'org-present-mode-hook
          (lambda ()
            (org-present-big)
            (org-display-inline-images)
            (org-present-hide-cursor)
            (org-present-read-only)))

(add-hook 'org-present-mode-quit-hook
          (lambda ()
            (org-present-small)
            (org-remove-inline-images)
            (org-present-show-cursor)
            (org-present-read-write)))
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
  (when (and (boundp 'org-mode-map)
             (boundp 'verb-command-map))
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))
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
(straight-use-package 'org-notify)

(with-eval-after-load 'org
  (require 'org-notify)

  (declare-function org-notify-start "ext:org-notify")
  (declare-function org-notify-add "ext:org-notify")

  (org-notify-start 60)

  (org-notify-add
   'default
   '(:time "10m" :period "2m" :duration 25 :actions -notify/window)
   '(:time "1h" :period "15m" :duration 25 :actions -notify/window)
   '(:time "2h" :period "30m" :duration 25 :actions -notify/window)))
;; =============================================================================

;; Org project
;; =============================================================================
(straight-use-package
 '(org-project :type git :host github :repo "delehef/org-project"))

(customize-set-variable 'org-project-todos-per-project t)

(declare-function org-project-quick-capture "ext:org-project")
(declare-function org-project-capture "ext:org-project")
(declare-function org-project-open-todos "ext:org-project")

(with-eval-after-load 'project
  (when (boundp 'project-prefix-map)
    (define-key project-prefix-map (kbd "t") #'org-project-quick-capture)
    (define-key project-prefix-map (kbd "T") #'org-project-capture)
    (define-key project-prefix-map (kbd "o") #'org-project-open-todos)))
;; =============================================================================

;; Org contacts
;; =============================================================================
(straight-use-package 'org-contacts)

(with-eval-after-load 'org
  (require 'org-contacts))
;; =============================================================================

;; Latex
;; =============================================================================
;; Abntex2 class
(with-eval-after-load 'ox-latex
  (when (boundp 'org-latex-classes)
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
                   ("\\maketitle" . "\\imprimircapa")))))

;; Source code highlight with Minted package.
(customize-set-variable 'org-latex-listings 'minted)
(customize-set-variable 'org-latex-packages-alist '(("" "minted")))
(customize-set-variable
 'org-latex-pdf-process
 '("latexmk -shell-escape -pdf -interaction=nonstopmode -file-line-error %f"))
;; =============================================================================

;; Lite Clickup org integration
;; =============================================================================
(defcustom vs/org-clickup-token-entry nil
  "Entry name on the password store to get the clickup token."
  :type 'string
  :group 'org
  :safe #'stringp)

(declare-function password-store-get "ext:password-store")
(declare-function org-edit-headline "ext:org")
(declare-function org-at-heading-p "ext:org")
(declare-function org-entry-get-p "ext:org")
(declare-function org-heading-components "ext:org")
(declare-function org-set-property "ext:org")

(defun vs/org-fill-clickup-task ()
  "Fill Clickup task."
  (interactive)
  (when-let* ((pt (point))
              (task-id (and (org-at-heading-p)
                            (car (split-string
                                  (nth 4 (org-heading-components))
                                  " ")))))
    (let-alist (vs/org-clickup-get-task task-id)
      (let ((headline (format "%s - %s" .id .name)))
        (message "Updating %s" headline)
        (org-edit-headline headline)
        (cl-loop
         for (property value)
         on (list
             "ClickupAssignee" (let-alist (nth 0 .assignees)
                                 .username)
             "ClickupCreated" .date_created
             "ClickupTaskId" .id
             "ClickupCreator" .creator.username
             "ClickupStatus" .status.status
             "ClickupDescription" .description)
         by #'cddr
         do (org-set-property property value))))))

(defun vs/org-clickup-get-task (task-id)
  "Get Clickup task associated to TASK-ID."
  (unless (executable-find "curl")
    (error "CURL is missing."))
  (let* ((url (format "https://api.clickup.com/api/v2/task/%s/" task-id))
         (auth-header (format "Authorization: %s"
                              (password-store-get vs/org-clickup-token-entry)))
         (command (format "curl -s -H \"%s\" '%s'" auth-header url)))
    (json-parse-string (shell-command-to-string command)
                       :object-type 'alist
                       :array-type 'list
                       :null-object nil)))
;; =============================================================================


(provide 'layer-org)

;;; layer-org.el ends here
