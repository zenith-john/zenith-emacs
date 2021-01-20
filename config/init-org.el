;;; init-org.el --- org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Majority of the file from
;;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/*.el
;; Changed org defaults (should be set before org loads)

;; Code:

;; org-mode
(defvar org-directory "~/Dropbox/")
(defvar org-agenda-files '("~/Dropbox/"))
(defvar zenith/note-directory (expand-file-name "~/Documents/Notes/"))
(defvar zenith/bibtex-library (expand-file-name "~/Dropbox/Library.bib")
  "The default bibtex library")

;;
;;; Packages

;; delay load org-mode
(zenith/delay-load (lambda ()(require 'org)))

;; org-clock
(zenith/autoload '(org-clock-load org-clock-save) "org-clock")
(add-hook 'org-mode-hook 'org-clock-load)
(setq org-clock-persist 'history
      org-clock-persist-file (concat zenith-emacs-local-dir "org-clock-save.el"))
(add-hook 'kill-emacs-hook #'org-clock-save)

;;
;;; Bootstrap
(defun zenith/org-mode-hook ()
  (visual-line-mode)
  ;; cdlatex
  ;; Enable cdlatex mode
  ;; TODO configure cdlatex-command-alist
  (setq-local company-idle-delay nil)
  (display-line-numbers-mode 0)
  (org-cdlatex-mode 1)
  ;; ox-hugo
  ;; dependencies: org
  (require 'ox-hugo)
  ;; org-bullet
  (require 'org-bullets)
  (org-bullets-mode)
  ;; Org-agenda export to icalendar
  (require 'ox-icalendar)
  ;; org-edit-latex
  (require 'org-edit-latex)
  (org-edit-latex-mode)
  ;; manually load org-id
  (require 'org-id)
  ;; load org-mind-map
  (require 'ox-org)
  (require 'org-mind-map))

(add-hook 'org-mode-hook 'zenith/org-mode-hook)

(with-eval-after-load 'org
  (defun zenith/refile-targets-notes ()
    (directory-files zenith/note-directory t ".*\\.org\\'"))
  (setq-default
   org-adapt-indentation nil
   org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
   org-cycle-include-plain-lists t
   org-catch-invisible-edits 'show-and-error
   org-entities-user
   '(("flat"  "\\flat" nil "" "" "266D" "♭")
     ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-goto-interface 'outline-path-completion
   org-hidden-keywords nil
   org-highlight-latex-and-related '(native)
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-id-track-globally t
   org-id-link-to-org-use-id t
   org-id-locations-file (expand-file-name ".org-id-locations" zenith-emacs-local-dir)
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-indent-mode-turns-on-hiding-stars t
   org-insert-heading-respect-content nil
   org-outline-path-complete-in-steps nil
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-priority-faces
   '((?a . error)
     (?b . warning)
     (?c . success))
   org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)
     (zenith/refile-targets-notes :maxlevel . 3))
   org-refile-use-outline-path 'file
   org-special-ctrl-a/e t
   org-src-fontify-natively t
   org-src-preserve-indentation t
   org-src-tab-acts-natively t
   org-src-window-setup 'split-window-below
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-use-sub-superscripts '{}

   ;; Scale up LaTeX previews a bit (default is too small)
   org-preview-latex-image-directory (concat zenith-emacs-local-dir "org-latex/")
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (setq-default
   org-format-latex-options
   (plist-put org-format-latex-options
              :background
              (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                  'default)
                              :background nil t)))

  ;; seems that default open method is not by default usable in org-file-apps
  (setcdr (assoc "\\.pdf\\'" org-file-apps) "/usr/bin/zathura %s")
  (add-to-list 'org-file-apps '(t . "nohup /usr/bin/xdg-open >/dev/null 2&>1 %s"))
  ;; Make emphasis clear when using bold font
  (add-to-list 'org-emphasis-alist
               '("*" (:foreground "pink")))
  (setq org-emphasis-regexp-components
        ;; markup 记号前后允许中文
        (list (concat " \t('\"{"            "[:nonascii:]")
              (concat "- \t.,:!?;'\")}\\["  "[:nonascii:]")
              " \t\r\n,\"'"
              "."
              1)
        org-match-substring-regexp
        (concat
         ;; 限制上标和下标的匹配范围，org 中对其的介绍见：(org) Subscripts and superscripts
         "\\([0-9a-zA-Zα-γΑ-Ω]\\)\\([_^]\\)\\("
         "\\(?:" (org-create-multibrace-regexp "{" "}" org-match-sexp-depth) "\\)"
         "\\|"
         "\\(?:" (org-create-multibrace-regexp "(" ")" org-match-sexp-depth) "\\)"
         "\\|"
         "\\(?:\\*\\|[+-]?[[:alnum:].,\\]*[[:alnum:]]\\)\\)"))
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "PAUSE(p)" "SOMEDAY(s)" "NEXT(n)" "|" "DONE(d!)" "CANCELLED(c@)")
          (sequence "[ ](T)" "[-](P)" "[?](m)" "|" "[X](D)"))
        org-todo-keyword-faces
        '(("[-]" :inherit (font-lock-constant-face bold))
          ("[?]" :inherit (warning bold))
          ("WAITING" :inherit bold)
          ("LATER" :inherit (warning bold))))

  (setq org-capture-templates
        '(
          ("h" "Homework" entry (file+headline "~/Dropbox/task.org"  "Homework")
           "* TODO %? :Homework:\n%U\n")
          ("s" "Schedule" entry (file+headline "~/Dropbox/task.org" "Schedule")
           "* %?\n%U\n")
          ("r" "Project" entry (file+headline "~/Dropbox/task.org" "Project")
           "* TODO %?\n%U\n")
          ("q" "Question" entry (file+headline "~/Dropbox/task.org" "Question")
           "* TODO %? :Question:\n%U\n")
          ("d" "Idea" entry (file+headline "~/Dropbox/task.org" "Idea")
           "* TODO %? :Idea:\n%U\n")))

  ;; Org tag
  (setq org-tag-alist
        '(("Improvement" . ?i)
          (:startgrouptag)
          ("Must")
          (:grouptags)
          ("Homework" . ?h)
          ("Job" . ?j)
          (:endgrouptag)
          ("Personal" . ?p)
          ("Question" . ?q)
          ("Idea" . ?d)))

  ;; Org habit
  (require 'org-habit)
  (setq org-habit-show-habits-only-for-today nil)

  ;; Org agenda settings
  (setq org-agenda-start-on-weekday nil
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-span 7
        org-agenda-compact-blocks t
        org-agenda-show-all-dates nil
        org-deadline-warning-days 365
        org-agenda-show-future-repeats 'next
        org-agenda-window-setup 'only-window)


  (setq org-agenda-custom-commands
        '(("b" "Agenda View" ((tags "AGENDAHEADER"
                                    ((org-agenda-overriding-header "Today's Schedule:")))
                              (agenda ""
                                      ((org-agenda-show-all-dates t)
                                       (org-agenda-span 'day)
                                       (org-deadline-warning-days 0)
                                       (org-agenda-start-day "+0d")))
                              (todo "NEXT"
                                    ((org-agenda-overriding-header "========================================\nNext Tasks:")))
                              (tags-todo "Must/!-NEXT"
                                         ((org-agenda-overriding-header "========================================\nMust Do:")))
                              (tags "BEFOREWEEKGLANCE"
                                    ((org-agenda-overriding-header "========================================\nNext Week Glance:")))
                              (agenda ""
                                      ((org-agenda-show-all-dates t)
                                       (org-agenda-show-future-repeats t)
                                       (org-agenda-span 6)
                                       (org-agenda-start-day "+1d")))
                              (tags "BEFOREDEADLINE"
                                    ((org-agenda-overriding-header "========================================\nFar Away Tasks:")))
                              (agenda ""
                                      ((org-agenda-span 180)
                                       (org-agenda-time-grid nil)
                                       (org-agenda-show-all-dates nil)
                                       (org-agenda-entry-types '(:deadline :scheduled))
                                       (org-agenda-start-day "+7d")))))
          ("i" "Improvement" ((tags-todo "Question"
                                         ((org-agenda-overriding-header "Unsolved Questions:")))
                              (tags-todo "Improvement" ((org-agenda-overriding-header "\n\nImprovment:")))
                              (tags-todo "Idea+TODO<>\"NEXT\"|Personal+TODO<>\"NEXT\""
                                         ((org-agenda-overriding-header "\n\nPersonal Project:")))))))

  (add-hook 'org-agenda-finalize-hook 'org-icalendar-combine-agenda-files)

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (shell . t))))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("myart"
                 "\\documentclass{article}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\usepackage{minted}
\\usepackage{fontspec}
\\usepackage{xeCJK}
\\usepackage{etoolbox}
\\usepackage[backend=biber,style=alphabetic]{biblatex}
\\addbibresource[location=local]{~/Dropbox/Library.bib}
\\setCJKmainfont{Source Han Sans CN}
\\setmonofont{Source Code Pro}
\\gappto{\\UrlBreaks}{\\UrlOrds}
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-compiler "xelatex"
        org-latex-default-class "myart"
        org-export-with-sub-superscripts nil
        org-latex-listings 'minted
        org-latex-remove-logfiles nil
        org-latex-minted-options '(("breaklines" "true")
                                   ("frame" "single")
                                   ("breakanywhere" "true")
                                   ("fontsize" "\\footnotesize"))
        org-latex-pdf-process
        '("latexmk -g -pdf -pdflatex=\"%latex\" -shell-escape -outdir=%o %f")))

;; org-edit-latex
(with-eval-after-load 'org-edit-latex
  (setq org-edit-latex-create-master nil))

;; org-mind-map
;; dependencies: dash org
(with-eval-after-load 'org-mind-map
  (setq org-mind-map-engine "dot"
        org-mind-map-dot-output '("pdf" "png" "eps")
        org-mind-map-include-text nil
        org-mind-map-include-images nil)
  (setcdr (assoc "resolution" org-mind-map-default-graph-attribs) "200"))

;; ox-icalendar
(with-eval-after-load 'ox-icalendar
  (setq org-icalendar-combined-agenda-file (expand-file-name "~/Dropbox/agenda.ics")
        org-icalendar-include-todo t
        org-icalendar-use-deadline '(event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(event-if-not-todo todo-start)
        org-icalendar-alarm-time 15
        org-icalendar-store-UID t
        org-agenda-default-appointment-duration 90))

(with-eval-after-load 'org
  (require 'evil-org)
  (evil-org-set-key-theme)
  (add-hook 'org-mode-hook
            'evil-org-mode))

(with-eval-after-load 'org-id
  (setq org-id-extra-files (directory-files-recursively zenith/note-directory ".*\\.org"))
  (org-id-update-id-locations)
  (defun org-id-complete-link (&optional arg)
    "Create an id: link using completion"
    (concat "id:"
            (org-id-get-with-outline-path-completion org-refile-targets)))
  (org-link-set-parameters "id"
                           :complete 'org-id-complete-link)
  (defun zenith/search-id-reverse-link ()
    "Search the id in the directory"
    (interactive)
    (let ((query
           (cdr (first (org-entry-properties nil "ID")))))
      (rg-project query "*.org")))
  (defun zenith/org-insert-link-by-id ()
    "Insert the link by id"
    (interactive)
    (let ((link (org-link--try-special-completion "id")))
      (org-insert-link nil link))))

(with-eval-after-load 'org-agenda
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(with-eval-after-load 'ol-bibtex
  ;; Redefine `org-bibtex-read-file' to avoid coding problem caused by loading as rawfile.
  (defun org-bibtex-read-file (file)
    "Read FILE with `org-bibtex-read-buffer'"
    (interactive "fFile: ")
    (org-bibtex-read-buffer (find-file-noselect file 'nowarn))))

;; Org attach
(setq org-attach-method 'lns)

(zenith/autoload '(org-attach org-attach-open) "org-attach")

(defun org-agenda-attach-open ()
  "Open attachment with one-key stroke."
  (interactive)
  (unless (eq major-mode 'org-agenda-mode)
    (let ((debug-on-quit nil))
      (signal 'quit '("This was written expressly for `*Org Agenda*`."))))
  (let ((marker (or (get-text-property (point) 'org-hd-marker)
                    (get-text-property (point) 'org-marker))))
    (if marker
        (save-excursion
          (set-buffer (marker-buffer marker))
          (goto-char marker)
          (org-back-to-heading t)
          (call-interactively 'org-attach-open))
      (error "No task in current line"))))

;; Global settings
(defun zenith/my-org-agenda ()
  (interactive)
  (org-agenda 0 "b"))

(setq org-ref-default-bibliography `(,zenith/bibtex-library)
      org-ref-bibliography-notes (concat zenith/note-directory "biblio.org")
      org-ref-completion-library 'org-ref-ivy-cite
      orhc-bibtex-cache-file (concat zenith-emacs-local-dir ".orhc-bibtex-cache"))

(zenith/delay-load 'zenith/require-org-ref-packages)

(defun zenith/require-org-ref-packages ()
  (interactive)
  (require 'org-ref)
  (require 'doi-utils)
  (require 'org-ref-isbn))

(provide 'init-org)
;;; init-org.el ends here
