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
(zenith/delay-load (lambda ()
                     (require 'org)
                     ;; org-wild-notifier.el
                     ;; dependencies: alert
                     (require 'org-wild-notifier)
                     (org-wild-notifier-mode)
                     ))

;; org-wild-notifier
(with-eval-after-load 'org-wild-notifier
  (require 'alert-toast)
  (setq org-wild-notifier-keyword-whitelist nil
        org-wild-notifier-alert-time '(15)
        alert-default-style 'toast))

;; org-clock
(zenith/autoload '(org-clock-load org-clock-save) "org-clock")
(add-hook 'org-mode-hook 'org-clock-load)
(setq org-clock-persist t
      org-clock-in-resume t
      org-clock-persist-query-resume nil
      org-clock-persist-file (concat zenith-emacs-local-dir "org-clock-save.el"))
(add-hook 'kill-emacs-hook #'org-clock-save)

(defun zenith/org-manual-clock ()
  (interactive)
  (org-clock-in)
  (org-clock-out))

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
  ;; load org-id
  (require 'org-id)
  (require 'org-edna)
  (org-edna-mode)
  ;; load org-mind-map
  (require 'ox-org)
  (require 'org-mind-map)
  (require 'org-download))

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
        '((sequence "TODO(t)" "WAITING(w@/!)" "NEXT(n)" "|" "DONE(d!)" "CANCELLED(c@)")))

  (setq org-capture-templates
        '(
          ("d" "Deadline" entry (file+headline "~/Dropbox/Agenda.org"  "Deadline")
           "* TODO %?\nDEADLINE:%^t\n%U\n")
          ("m" "Meeting" entry (file+headline "~/Dropbox/Agenda.org" "Meeting")
           "* TODO %?\n%^t\n%U\n")
          ("s" "Schedule" entry (file+headline "~/Dropbox/Agenda.org" "Schedule")
           "* TODO %?\nSCHEDULED:%^t\n%U\n")
          ("p" "Project" entry (file "~/Dropbox/Projects.org")
           "* TODO %?\n%U\n")
          ("n" "Notes" entry (file "~/Dropbox/Temp.org")
           "* %? \n%U\n")))

  ;; Org tag
  (setq org-tag-alist
        '(
          (:startgrouptag . nil)
          ("CRUCIAL" . ?c)
          ("Important" . ?i)
          ("Urgent" . ?u)
          (:endgrouptag . nil)))

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
        org-agenda-window-setup 'current-window)

  (setq org-agenda-custom-commands
        '(("b" "Agenda View" ((tags "AGENDAHEADER"
                                    ((org-agenda-overriding-header "Today's Schedule:")))
                              (agenda ""
                                      ((org-agenda-show-all-dates t)
                                       (org-agenda-use-time-grid t)
                                       (org-agenda-time-grid '((daily today require-timed remove-match)
                                                               (700 800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200 2300)
                                                               "......" "----Free----"))
                                       (org-agenda-span 'day)
                                       (org-deadline-warning-days 0)
                                       (org-agenda-start-day "+0d")))
                              (tags-todo "CRUCIAL"
                                         ((org-agenda-overriding-header "CRUCIAL:")))
                              (tags-todo "Urgent"
                                         ((org-agenda-overriding-header "Urgent:")))
                              (tags-todo "-CRUCIAL-Urgent/+NEXT"
                                    ((org-agenda-overriding-header "========================================\nNext Tasks:")))
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
          ("c" "Todo Lists"
           ((alltodo "" ((org-agenda-overriding-header "TODOs sorted by state, priority, effort")
                       (org-agenda-sorting-strategy '(todo-state-down priority-down effort-up))))))))

  (defun org-time-to-minutes (time)
    "Convert an HHMM time to minutes"
    (+ (* (/ time 100) 60) (% time 100)))

  (defun org-time-from-minutes (minutes)
    "Convert a number of minutes to an HHMM time"
    (+ (* (/ minutes 60) 100) (% minutes 60)))

  (defadvice org-agenda-add-time-grid-maybe (around mde-org-agenda-grid-tweakify
                                                    (list ndays todayp))
    (if (member 'remove-match (car org-agenda-time-grid))
        (flet ((extract-window
                (line)
                (let ((start (get-text-property 1 'time-of-day line))
                      (dur (get-text-property 1 'duration line)))
                  (cond
                   ((and start dur)
                    (cons start
                          (org-time-from-minutes
                           (truncate
                            (+ dur (org-time-to-minutes start))))))
                   (start start)
                   (t nil)))))
          (let* ((windows (delq nil (mapcar 'extract-window list)))
                 (org-agenda-time-grid
                  (list
                   (car org-agenda-time-grid)
                   (remove-if
                    (lambda (time)
                      (find-if (lambda (w)
                                 (if (numberp w)
                                     (equal w time)
                                   (and (>= time (car w))
                                        (< time (cdr w)))))
                               windows))
                    (cadr org-agenda-time-grid) )
                   (caddr org-agenda-time-grid)
                   (cadddr org-agenda-time-grid)
                   )))
            ad-do-it))
      ad-do-it))
  (ad-activate 'org-agenda-add-time-grid-maybe)

  (add-hook 'org-agenda-finalize-hook 'org-icalendar-combine-agenda-files)

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (shell . t)))

  (require 'org-tempo)
  (setq org-structure-template-alist
        '(("p" . "proof")
          ("q" . "quote")
          ("t" . "theorem")
          ("c" . "corollary")
          ("d" . "definition")
          ("P" . "proposition")
          ("s" . "src")
          ("C" . "comment"))))

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
        org-agenda-default-appointment-duration nil))

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

  (defun zenith/org-link-edna-id ()
    "Insert id as edna format"
    (interactive)
    (let ((link (org-link--try-special-completion "id")))
      (insert (concat "ids(" (nth 1 (split-string link ":"))  ")"))))

  (defun zenith/org-insert-link-by-id ()
    "Insert the link by id"
    (interactive)
    (let ((link (org-link--try-special-completion "id")))
      (org-insert-link nil link))))

(with-eval-after-load 'org-agenda
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)

  (defun zenith/past-time-p (timestamp)
    "Check whether the timestamp time is older than today."
    (> 0 (org-time-stamp-to-now timestamp)))

  (defun zenith/org-archive-old ()
    "Remove the old events in file without repeats."
    (let ((org-trust-scanner-tags t)
          (timestamp (org-entry-get nil "TIMESTAMP")))
      (when
          (and timestamp (zenith/past-time-p timestamp)
               (not (s-matches? "\\+" timestamp)))
        (setq org-map-continue-from (point))
        (org-archive-subtree))))

  (defun zenith/org-clean-agenda ()
    "Remove the old events without repeats in agenda."
    (interactive)
    (org-map-entries 'zenith/org-archive-old t 'agenda))

  (defun zenith/org-calc-interval (rep)
    "Calculate the interval in reps."
    (let ((day (string-to-number
                   (or (car (s-match "\\+[0-9]+d" rep)) "0")))
          (week (string-to-number
                   (or (car (s-match "\\+[0-9]+w" rep)) "0")))
          (month (string-to-number
                   (or (car (s-match "\\+[0-9]+m" rep)) "0")))
          (year (string-to-number
                   (or (car (s-match "\\+[0-9]+y" rep)) "0"))))
      (+ day (* 7 week) (* 28 month) (* 365 year))))

  (defun zenith/org-calc-clones (timestamp interval)
    "Calculate the clone numbers."
    (let ((time (org-time-stamp-to-now timestamp)))
      (max (/ (- (+ interval 7) time) interval) 0)))

  (defun -zenith/org-clone-repeats ()
    "Repeat the daily task 7 times"
    (when-let* ((org-trust-scanner-tags t)
              (timestamp (org-entry-get nil "TIMESTAMP"))
              (rep (car-safe (s-match "\\+[0-9]+[dwmy]" timestamp)))
              (interval (zenith/org-calc-interval rep))
              (times (zenith/org-calc-clones timestamp interval))
              (check (> times 0)))
      (save-restriction
        (org-narrow-to-subtree)
        (org-clone-subtree-with-time-shift times rep)
        (end-of-buffer))
      (org-next-visible-heading 1)
      (setq org-map-continue-from (point))))

  (defun zenith/org-clone-repeats ()
    "Repeat the daily tasks."
    (interactive)
    (org-map-entries '-zenith/org-clone-repeats t 'agenda))

  (defun zenith/org-next-week ()
    "Prepare org-mode for next week"
    (interactive)
    (zenith/org-clean-agenda)
    (zenith/org-clone-repeats)))

(with-eval-after-load 'ol-bibtex
  ;; Redefine `org-bibtex-read-file' to avoid coding problem caused by loading as rawfile.
  (defun org-bibtex-read-file (file)
    "Read FILE with `org-bibtex-read-buffer'"
    (interactive "fFile: ")
    (org-bibtex-read-buffer (find-file-noselect file 'nowarn))))

;; org-downloads configuration
(with-eval-after-load 'org-download
  (setq org-download-image-org-width 600)
  (setq-default org-download-image-dir "./img"
                org-download-heading-lvl nil))
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
