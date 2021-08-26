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
(require 'org)

;; org-wild-notifier.el
;; dependencies: alert
(zenith/autoload '(org-wild-notifier-mode) "org-wild-notifier")
(zenith/delay-load 'org-wild-notifier-mode)

;; org-wild-notifier
(with-eval-after-load 'alert
  (if (not zenith/wsl-system)
      (setq alert-default-style 'libnotify)
    (require 'burnt-toast-alert)
    (setq burnt-toast-powershell-command "powershell.exe"
          alert-default-style 'burnt-toast)))
(with-eval-after-load 'org-wild-notifier
  (setq org-wild-notifier-keyword-whitelist nil
        org-wild-notifier-alert-time '(15)))

;; org-ref
;; should be set before loading the package.
(setq org-ref-default-bibliography `(,zenith/bibtex-library)
      org-ref-bibliography-notes (concat zenith/note-directory "biblio.org")
      org-ref-completion-library 'org-ref-ivy-cite
      orhc-bibtex-cache-file (concat zenith-emacs-local-dir ".orhc-bibtex-cache"))
;;
;;; Bootstrap
(defun zenith/org-load-packages ()
  (require 'org-clock)
  (require 'ox-hugo)
  (require 'org-bullets)
  (require 'ox-icalendar)
  (require 'org-edit-latex)
  (require 'org-id)
  (require 'ox-org)
  (require 'org-download)
  (require 'org-ref)
  (require 'doi-utils)
  (require 'org-ref-isbn)
  (setq org-roam-v2-ack t)
  (require 'org-roam))

(zenith/delay-load 'zenith/org-load-packages)

(defun zenith/org-mode-hook ()
  (zenith/org-load-packages)
  (visual-line-mode)
  (display-line-numbers-mode 0)
  (org-bullets-mode)
  (org-edit-latex-mode)
  (org-cdlatex-mode 1)
  (org-clock-load)
  (require 'company-math)
  (remove-hook 'completion-at-point-functions #'pcomplete-completions-at-point t)
  (setq-local company-backends '(company-math-symbols-latex company-capf)))

(add-hook 'org-mode-hook 'zenith/org-mode-hook)

(with-eval-after-load 'org
  (defun zenith/refile-targets-notes ()
    (directory-files zenith/note-directory t ".*\\.org\\'"))

  (defun zenith/org-agenda-files ()
    (when-let ((agenda (car org-agenda-files)))
      (remove (expand-file-name "chinese_lunar.org" agenda)
              (directory-files agenda t ".*\\.org\\'"))))

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
   org-priority-default ?C
   org-priority-faces
   '((?A . error)
     (?B . warning)
     (?C . success))
   org-refile-targets
   '((nil :maxlevel . 3)
     (zenith/org-agenda-files :maxlevel . 3)
     (zenith/refile-targets-notes :maxlevel . 4))
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
        ;; TODO means the task that requires work.
        ;; SUSPEND means the work that is not continued by various reasons.
        ;; WAITING means the work is deferred and I am waiting for reply.
        ;; ACTIVE means the project is under construction but requires a lot of time
        ;; and is not so clear.
        ;; NEXT means the task is under my work and is immediate (compare to ACTIVE).
        ;; DONE means that the task is finished.
        ;; CANCELLED means that the work will not be continued anymore.
        '((sequence "TODO(t)" "SUSPEND(s)" "WAITING(w@/!)" "ACTIVE(a)" "NEXT(n)" "|" "DONE(d!)" "CANCELLED(c@)")))

  ;; Use org-protocol to capture web.
  (require 'org-protocol)
  (setq org-capture-templates
        '(
          ("d" "Deadline" entry (file+headline "~/Dropbox/Agenda.org"  "Deadline")
           "* TODO %^{Title} :Work:\nDEADLINE: %^t\n%U\n%?")
          ("m" "Meeting" entry (file+headline "~/Dropbox/Agenda.org" "Meeting")
           "* %^{Title} :Work:\n%^t\n%U\n%?")
          ("a" "Mails" entry (file+headline "~/Dropbox/Agenda.org" "Meeting")
           "* TODO %^{Title} :Work:\n%U\n%a\n%?")
          ("s" "Schedule" entry (file+headline "~/Dropbox/Agenda.org" "Schedule")
           "* TODO %^{Title} :Work:\nSCHEDULED: %^t\n%U\n%?")
          ("p" "Project" entry (file "~/Dropbox/Projects.org")
           "* TODO %^{Title}\n%U\n%?")
          ("n" "Notes" entry (file "~/Dropbox/Temp.org")
           "* %^{Title}\n%U\n%?")
          ("o" "Todo Notes" entry (file "~/Dropbox/Temp.org")
           "* %?\nSCHEDULED: %t\n%U\n")
          ("t" "Daily Review" entry (file "~/Dropbox/Temp.org")
           "* %(format-time-string \"%Y-%m-%d\") Daily Review\n%U\n%?" :immediate-finish t :jump-to-captured t)
          ("r" "Reference" entry (file "~/Dropbox/Temp.org")
           "* %^{Title}\n%U\nAuthor: %^{Author}%?")
          ("x" "Web Capture" entry (file "~/Dropbox/Temp.org")
           "* %:description\n:PROPERTIES:\n:ROAM_REFS: %:link\n:END:\n%U\n%:annotation\n%i\n%?")))

  ;; Create id when capture ends.
  (add-hook 'org-capture-prepare-finalize-hook 'org-id-get-create)

  (defun zenith/org-insert-heading ()
    "Insert heading, create id and add a timestamp."
    (interactive)
    (let ((org-capture-templates
           '(
             ("n" "Notes" entry (file "~/Dropbox/Temp.org")
              "* %^{Title}\n%U\n" :immediate-finish t)
             )))
      (org-capture 0 "n")))

  ;; Org tag
  (setq org-tag-alist
        '(
          (:startgroup . nil)
          ("Node" . ?n)
          ("Ignore" . ?g)
          (:endgroup . nil)
          (:startgroup . nil)
          ("Work" . ?w)
          ("Personal" . ?e)
          (:endgroup . nil)
          ("Question" . ?q)
          ("Urgent" . ?u)
          ("Project" . ?p)))

  ;; Org agenda settings
  (setq org-agenda-start-on-weekday nil
        org-agenda-skip-scheduled-if-deadline-is-shown nil
        org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-span 7
        org-agenda-compact-blocks t
        org-agenda-show-all-dates nil
        org-agenda-tags-todo-honor-ignore-options t
        org-agenda-todo-ignore-with-date t
        org-deadline-warning-days 365
        org-agenda-show-future-repeats t
        org-agenda-window-setup 'current-window)

  (setq org-agenda-custom-commands
        '(("b" "Today" ((tags "AGENDAHEADER"
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
                              (tags-todo "/+NEXT"
                                         ((org-agenda-overriding-header "========================================\nNext Tasks:")))
                              (tags-todo "/+ACTIVE"
                                         ((org-agenda-overriding-header "Active Projects:")))
                              (tags-todo "Urgent/-NEXT"
                                         ((org-agenda-overriding-header "Works")))
                              (tags-todo "Work-Urgent/-Next"
                                         ((org-agenda-overriding-header "")))
                              (tags "BEFOREWEEKGLANCE"
                                    ((org-agenda-overriding-header "========================================\nTomorrow Glance:")))
                              (agenda ""
                                      ((org-agenda-show-all-dates t)
                                       (org-agenda-show-future-repeats t)
                                       (org-agenda-span 1)
                                       (org-agenda-start-day "+1d")))
                              (tags "BEFOREDEADLINE"
                                    ((org-agenda-overriding-header "========================================\nFar Away Tasks:")))
                              (agenda ""
                                      ((org-agenda-show-future-repeats 'next)
                                       (org-agenda-span 180)
                                       (org-agenda-time-grid nil)
                                       (org-agenda-show-all-dates nil)
                                       (org-agenda-entry-types '(:deadline :scheduled))
                                       (org-agenda-start-day "+2d")))))
          ("c" "Todo Lists"
           ((alltodo "" ((org-agenda-overriding-header "TODOs sorted by state, priority, effort")
                         (org-agenda-sorting-strategy '(todo-state-down priority-down effort-up))))))))

  ;; org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (shell . t)))

  (setq org-structure-template-alist
        '(("p" . "proof")
          ("q" . "quote")
          ("t" . "theorem")
          ("c" . "corollary")
          ("d" . "definition")
          ("l" . "lemma")
          ("P" . "proposition")
          ("s" . "src")
          ("C" . "comment")))

  (require 'evil-org)
  (evil-org-set-key-theme)
  (add-hook 'org-mode-hook
            'evil-org-mode)

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

  (org-link-set-parameters "zotero" :follow
                         (lambda (zpath)
                           (browse-url
                            ;; we get the "zotero:"-less url, so we put it back.
                            (format "zotero:%s" zpath))))

  (defun zenith/org-link-at-point ()
    "Return the substring of the raw text of link at the point."
    (let ((bound (org-in-regexp org-link-bracket-re 1)))
      (buffer-substring-no-properties (car bound) (cdr bound))))

  (defun zenith/copy-link-at-point ()
    "Copy the raw text of the link at point."
    (interactive)
    (kill-new (zenith/org-link-at-point))))

;; org-clock
(with-eval-after-load 'org-clock
  (setq org-clock-persist t
        org-clock-in-resume t
        org-clock-persist-query-resume nil
        org-clock-persist-file (concat zenith-emacs-local-dir "org-clock-save.el"))
  (add-hook 'kill-emacs-hook #'org-clock-save)

  (defun zenith/org-manual-clock ()
    (interactive)
    (org-clock-in)
    (org-clock-out)))

;; ox-latex
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

;; ox-hugo
(with-eval-after-load 'ox-hugo
  (setq org-hugo-base-dir "~/Documents/zenith-john.github.io/"
        org-hugo-section "post"))

;; ox-icalendar
(with-eval-after-load 'ox-icalendar
  (setq org-icalendar-combined-agenda-file (expand-file-name "~/Dropbox/agenda.ics")
        org-icalendar-include-todo t
        org-icalendar-use-deadline '(event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(event-if-not-todo todo-start)
        org-icalendar-alarm-time 15
        org-icalendar-store-UID t
        org-agenda-default-appointment-duration nil))

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

  (defun zenith/org-id-get-headline (id)
    "Get the headline of the given id."
    (save-window-excursion
      (save-excursion
        (org-id-open id "")
        (org-back-to-heading)
        (when (looking-at org-complex-heading-regexp)
          (match-string-no-properties 4)))))

  (defun zenith/org-insert-link-by-id ()
    "Insert the link by id"
    (interactive)
    (let* ((link (org-link--try-special-completion "id"))
           (desc (zenith/org-id-get-headline (substring link 3))))
      (org-insert-link nil link desc)))

  (defun zenith/org-insert-link ()
    "Insert the stored link or by id"
    (interactive)
    (if org-stored-links
        (org-insert-last-stored-link 1)
      (zenith/org-insert-link-by-id))))

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
    (zenith/org-clone-repeats))

  (defun org-time-to-minutes (time)
    "Convert an HHMM time to minutes"
    (+ (* (/ time 100) 60) (% time 100)))

  (defun org-time-from-minutes (minutes)
    "Convert a number of minutes to an HHMM time"
    (+ (* (/ minutes 60) 100) (% minutes 60)))

  (defun zenith/extract-window (line)
    (let ((start (get-text-property 1 'time-of-day line))
          (dur (get-text-property 1 'duration line)))
      (cond
       ((and start dur)
        (cons start
              (org-time-from-minutes
               (truncate
                (+ dur (org-time-to-minutes start))))))
       (start start)
       (t nil))))

  (defadvice org-agenda-add-time-grid-maybe (around mde-org-agenda-grid-tweakify
                                                    (list ndays todayp))
    (if (member 'remove-match (car org-agenda-time-grid))
        (let* ((windows (delq nil (mapcar 'zenith/extract-window list)))
               (org-agenda-time-grid
                (list
                 (car org-agenda-time-grid)
                 (cl-remove-if
                  (lambda (time)
                    (cl-find-if (lambda (w)
                                  (if (numberp w)
                                      (equal w time)
                                    (and (>= time (car w))
                                         (< time (cdr w)))))
                                windows))
                  (cadr org-agenda-time-grid))
                 (caddr org-agenda-time-grid)
                 (cadddr org-agenda-time-grid)
                 )))
          ad-do-it)
      ad-do-it))
  (ad-activate 'org-agenda-add-time-grid-maybe)

  (add-hook 'org-agenda-finalize-hook 'org-icalendar-combine-agenda-files))

(with-eval-after-load 'ol-bibtex
  ;; Redefine `org-bibtex-read-file' to avoid coding problem caused by loading as rawfile.
  (defun org-bibtex-read-file (file)
    "Read FILE with `org-bibtex-read-buffer'"
    (interactive "fFile: ")
    (org-bibtex-read-buffer (find-file-noselect file 'nowarn))))

;; org-downloads configuration
(with-eval-after-load 'org-download
  (if zenith/wsl-system
      (setq org-download-screenshot-method
            "i_view64.exe /capture=4 /convert=\"D:\\\\screenshot.png\"; mv /mnt/d/screenshot.png %s")
    (setq org-download-screenshot-method "flameshot gui --raw > %s"))
  (setq org-download-image-org-width 600)
  (setq-default org-download-image-dir "./img"
                org-download-heading-lvl nil))

;; org-roam configuration
;; In fact I only use org-roams backlink feature
(with-eval-after-load 'org-roam
  (setq org-roam-directory zenith/note-directory
        org-roam-db-location (expand-file-name "org-roam.db" zenith-emacs-local-dir)
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-db-update-on-save nil
        org-roam-completion-everywhere nil))

;; Global settings
(defun zenith/my-org-agenda ()
  (interactive)
  (org-agenda 0 "b"))

(provide 'init-org)
;;; init-org.el ends here
