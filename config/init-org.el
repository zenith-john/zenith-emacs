;;; init-org.el --- org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Majority of the file from
;;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/*.el
;; Changed org defaults (should be set before org loads)

;; Code:

;; org-mode
(defvar org-directory "~/Dropbox/")
(defvar zenith/org-agenda-files '("~/Dropbox/"))
(defvar org-agenda-files zenith/org-agenda-files)
(defvar zenith/note-directory (expand-file-name "~/Documents/Notes/"))
(defvar zenith/bibtex-library (expand-file-name "~/Dropbox/Library.bib")
  "The default bibtex library")
(defvar zenith/org-roam-capture-id nil
  "Store the id of the capture template.")

;;
;;; Packages

(require 'org)

;; Citation by citeproc
(require 'oc-csl)
(setq org-cite-global-bibliography `(,zenith/bibtex-library)
      org-cite-csl-styles-dir "~/Zotero/styles/"
      org-cite-export-processors
      '((html . (csl "journal-of-combinatorics.csl"))
        (md . (csl "journal-of-combinatorics.csl"))   ; Footnote reliant
        (latex . biblatex)                                 ; For humanities
        (odt . (csl "chicago-fullnote-bibliography.csl"))  ; Footnote reliant
        (t . (csl "modern-language-association.csl"))))    ; Fallback

(require 'citar-org)
(setq org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar
      citar-bibliography org-cite-global-bibliography)

;;
;;; Bootstrap
(defun zenith/org-load-packages ()
  (require 'ob-async)
  (require 'org-bullets)
  (require 'org-clock)
  (require 'org-download)
  (require 'org-edit-latex)
  (require 'org-id)
  (require 'org-roam)
  (require 'ox-hugo)
  (require 'ox-icalendar)
  (require 'ox-org))

(zenith/delay-load 'zenith/org-load-packages)

(defun zenith/org-mode-hook ()
  (zenith/org-load-packages)
  (visual-line-mode)
  (display-line-numbers-mode 0)
  (org-bullets-mode)
  (org-edit-latex-mode)
  (org-cdlatex-mode 1)
  (eldoc-mode 0)
  (org-clock-load)
  (require 'company-math)
  (remove-hook 'completion-at-point-functions #'pcomplete-completions-at-point t)
  (setq-local company-backends '(company-math-symbols-latex company-capf)))

(add-hook 'org-mode-hook 'zenith/org-mode-hook)

(with-eval-after-load 'org
  (defun zenith/refile-targets-notes ()
    (directory-files zenith/note-directory t ".*\\.org\\'"))

  (defun zenith/org-agenda-files ()
    (when-let ((agenda (car zenith/org-agenda-files)))
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

  ;; Set org-format-latex-options
  (setq-default
   org-format-latex-options
   (plist-put org-format-latex-options
              :background
              (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                  'default)
                              :background nil t)))
  (setq-default
   org-format-latex-options
   (plist-put org-format-latex-options
              :foreground
              (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                  'default)
                              :foreground nil t)))

  ;; Because of the double screen with different dpi, I set it manually.
  (defun zenith/org--get-diplay-dpi-advice (orig-fn)
    "Make `org--get-display-dpi' work for non graphic mode"
    ;; manually set default dpi
    96)
  (advice-add 'org--get-display-dpi :around 'zenith/org--get-diplay-dpi-advice)

  ;; seems that default open method is not by default usable in org-file-apps
  (setcdr (assoc "\\.pdf\\'" org-file-apps) "/usr/bin/zathura %s")
  (add-to-list 'org-file-apps '(t . "nohup /usr/bin/xdg-open >/dev/null 2&>1 %s"))
  ;; Make emphasis clear when using bold font
  (add-to-list 'org-emphasis-alist
               '("*" (:foreground "pink")))
  (setq org-emphasis-regexp-components
        ;; markup 记号前后允许中文
        (list (concat " \t('\""            "[:nonascii:]")
              (concat "- \t.,:!?;'\")\\["  "[:nonascii:]")
              " \t\r\n,\"'*"
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
        '((sequence "TODO(t)" "Maybe(m)" "SUSPEND(s)" "WAITING(w!)" "ACTIVE(a)" "NEXT(n)" "|" "DONE(d!)" "CANCELLED(c@)")))

  ;; Use org-protocol to capture web.
  (require 'org-protocol)
  (setq org-capture-templates
        '(
          ("o" "Todo Notes" entry (file "~/Dropbox/Temp.org")
           "* TODO %^{Title}\n%U\n")
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
          ("t" "Daily Review" entry (file "~/Dropbox/Temp.org")
           "* %(format-time-string \"%Y-%m-%d\") Daily Review\n%U\n%?" :immediate-finish t :jump-to-captured t)
          ("r" "Reference" entry (file "~/Dropbox/Temp.org")
           "* %^{Title}\n%U\nAuthor: %^{Author}%?")
          ("x" "Web Capture" entry (file "~/Dropbox/Temp.org")
           "* %:description\n:PROPERTIES:\n:ROAM_REFS: %:link\n:END:\n%U\n%:annotation\n%i\n%?")))

  ;; Create id when capture ends.
  (defun zenith/org-id-get-create (&optional force)
    (interactive "P")
    (let ((id (org-id-get-create)))
      (when (org-roam-capture-p)
        (org-roam-capture--put :id (org-id-get-create force))
        (org-roam-capture--put :finalize (or (org-capture-get :finalize)
                                             (org-roam-capture--get :finalize))))))
  (add-hook 'org-capture-prepare-finalize-hook 'zenith/org-id-get-create)

  (defun zenith/org-insert-decoration ()
    "Insert the inactive timestamp and add id."
    (interactive)
    (org-time-stamp-inactive '(16))
    (org-id-get-create)
    (newline))

  (defun zenith/org-insert-heading ()
    "Insert heading for org-roam."
    (interactive)
    (org-insert-heading)
    (save-excursion
      (newline)
      (zenith/org-insert-decoration)))

  (defun zenith/org-ctrl-c-ret (&optional arg)
    "Insert heading, create id and add a timestamp."
    (interactive "p")
    (pcase arg
      (4 (zenith/org-insert-heading))
      (_ (call-interactively 'org-ctrl-c-ret))))

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
        org-agenda-time-grid '((daily today require-timed remove-match)
                               (800 1000 1200 1400 1600 1800 2000 2200)
                               "......" "----Free----")

        org-deadline-warning-days 365
        org-agenda-show-future-repeats t
        org-agenda-window-setup 'current-window)

  (setq org-agenda-custom-commands
        '(("b" "Today" ((tags "AGENDAHEADER"
                              ((org-agenda-overriding-header "Today's Schedule:")))
                        (agenda ""
                                ((org-agenda-show-all-dates t)
                                 (org-agenda-use-time-grid t)
                                 (org-agenda-span 'day)
                                 (org-agenda-start-day "+0d")))
                        (tags-todo "/+NEXT"
                                   ((org-agenda-overriding-header "========================================\nNext Tasks:")))
                        (tags-todo "/+ACTIVE"
                                   ((org-agenda-overriding-header "Active Projects:")))
                        (tags-todo "/+WAITING"
                                   ((org-agenda-overriding-header "")))
                        (tags-todo "Urgent/-NEXT"
                                   ((org-agenda-overriding-header "Works")))
                        (tags-todo "Work-Urgent/-Next"
                                   ((org-agenda-overriding-header "")))
                        (tags "BEFOREWEEKGLANCE"
                              ((org-agenda-overriding-header "========================================\nTomorrow Glance:")))
                        (agenda ""
                                ((org-agenda-show-all-dates t)
                                 (org-agenda-show-future-repeats t)
                                 (org-deadline-warning-days 0)
                                 (org-agenda-span 1)
                                 (org-agenda-start-day "+1d")))
                        (tags "BEFOREDEADLINE"
                              ((org-agenda-overriding-header "========================================\nFar Away Tasks:")))
                        (agenda ""
                                ((org-agenda-show-future-repeats 'next)
                                 (org-agenda-span 7)
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
     (dot . t)
     (shell . t)))

  ;; Define mathemtaical environment for emacs
  (setq org-structure-template-alist
        '(("p" . "proof")
          ("q" . "quote")
          ("r" . "remark")
          ("t" . "theorem")
          ("c" . "corollary")
          ("e" . "exam")
          ("j" . "conjecture")
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
                 "\\documentclass{ctexart}
[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\usepackage{minted}
\\usepackage{fontspec}
\\usepackage{xeCJK}
\\usepackage{etoolbox}
\\usepackage{indentfirst}
\\setCJKmainfont{Noto Sans CJK SC}
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
        '("latexmk -g -pdf -pdflatex=\"%latex\" -shell-escape -outdir=%o %f"))
  (defun zenith/org-latex-timestamp-advice (origin-fn timestamp _contents info)
    (if org-export-with-timestamps
        (funcall origin-fn timestamp _contents info)
      ""))

  (advice-add 'org-latex-timestamp :around 'zenith/org-latex-timestamp-advice))

;; org-edit-latex
(with-eval-after-load 'org-edit-latex
  (setq org-edit-latex-create-master nil))

;; ox-hugo
(with-eval-after-load 'ox-hugo
  (setq org-hugo-base-dir "~/Documents/zenith-john.github.io/"
        org-hugo-section "post"
        org-hugo-default-static-subdirectory-for-externals "img"
        org-hugo-paired-shortcodes "%theorem %corollary %proof %proposition %lemma %conjecture %remark %exam %definition"
        org-export-with-timestamps nil)

  ;; Redefine `org-hugo-link--headline-anchor-maybe' to remove the `kill-buffer' function.
  (defun org-hugo-link--headline-anchor-maybe (link)
    "Return headline pointed to by LINK."
    (with-temp-buffer
      (org-id-goto (org-element-property :path link))
      ;; Thu Oct 21 21:29:17 EDT 2021 - kmodi
      ;; It was necessary to set the major mode to `org-mode' after the
      ;; `org-id-goto' jump. Otherwise, the temp buffer would remain
      ;; in fundamental mode, and so the `org-find-top-headline'
      ;; always returned nil.
      (org-mode)
      (let ((headline (org-find-top-headline)))
        ;; (kill-buffer (current-buffer))
        (if headline
            (org-hugo-slug headline)
          ""))))
  ;; ox-hugo requires the level of first heading the to be 2.
  (defun zenith/org-hugo-export-advice (orig-fn &rest args)
    ;; Advice the ox-hugo export to use another version of bibliography.
    (let ((orig (symbol-function 'org-ref-get-md-bibliography)))
      (cl-letf (((symbol-function 'org-ref-get-md-bibliography)
                 'zenith/org-ref-get-md-bibliography))
        (apply orig-fn args))))
  (advice-add 'org-hugo-export-to-md :around 'zenith/org-hugo-export-advice)
  (advice-add 'org-hugo-export-as-md :around 'zenith/org-hugo-export-advice)
  (advice-add 'org-hugo-export-wim-to-md :around 'zenith/org-hugo-export-advice)

  (defun zenith/org-blackfriday-escape-chars-in-equation-advice (str)
    "Replace the newline by space for katex."
    (let* ((escape-str (replace-regexp-in-string "\n" " " str)))
      escape-str))
  (advice-add 'org-blackfriday-escape-chars-in-equation :filter-return 'zenith/org-blackfriday-escape-chars-in-equation-advice)

  (defun zenith/org-ref-format-cite-advice (origin-fn keyword desc format)
    "Add advice to change the citation format for markdown output."
    (if (eq format 'md)
        (concat "["
                (mapconcat
                 (lambda
                   (key)
                   (format "<span id=\"%s\"><a href=\"#%s\" title=\"%s\">%s</a></span>"
                           (md5 key)
                           key
                           (let
                               ((org-ref-bibliography-files
                                 (org-ref-find-bibliography))
                                (file)
                                (entry)
                                (bibtex-entry)
                                (entry-type)
                                (format)
                                (org-ref-bibliography-entry-format
                                 '(("article" . "%a, %t, %j, v(%n), %p (%y).")
                                   ("book" . "%a, %t, %u (%y).")
                                   ("techreport" . "%a, %t, %i, %u (%y).")
                                   ("proceedings" . "%e, %t in %S, %u (%y).")
                                   ("inproceedings" . "%a, %t, %p, in %b, edited by %e, %u (%y)"))))
                             (setq file
                                   (catch 'result
                                     (cl-loop for file in org-ref-bibliography-files do
                                              (if
                                                  (org-ref-key-in-file-p key
                                                                         (file-truename file))
                                                  (throw 'result file)
                                                (message "%s not found in %s" key
                                                         (file-truename file))))))
                             (if file
                                 (with-temp-buffer
                                   (insert-file-contents file)
                                   (bibtex-set-dialect
                                    (parsebib-find-bibtex-dialect)
                                    t)
                                   (bibtex-search-entry key nil 0)
                                   (setq bibtex-entry
                                         (bibtex-parse-entry))
                                   (dolist
                                       (cons-cell bibtex-entry)
                                     (setf
                                      (car cons-cell)
                                      (downcase
                                       (car cons-cell))))
                                   (setq entry-type
                                         (downcase
                                          (cdr
                                           (assoc "=type=" bibtex-entry))))
                                   (setq format
                                         (cdr
                                          (assoc entry-type org-ref-bibliography-entry-format)))
                                   (if format
                                       (setq entry
                                             (org-ref-reftex-format-citation bibtex-entry format))
                                     (save-restriction
                                       (bibtex-narrow-to-entry)
                                       (setq entry
                                             (buffer-string)))))
                               "Key not found")
                             (replace-regexp-in-string "\"" ""
                                                       (htmlize-escape-or-link entry)))
                           key))
                 (s-split "," keyword)
                 ", ")
                "]")
      (funcall origin-fn keyword desc format)))
  (advice-add 'org-ref-format-cite :around 'zenith/org-ref-format-cite-advice)

  (defun zenith/org-ref-get-md-bibliography (&optional sort)
    "Create an md bibliography when there are keys.
if SORT is non-nil the bibliography is sorted alphabetically by key."
    (let ((keys (org-ref-get-bibtex-keys sort)))
      (when keys
        (concat
         (mapconcat (lambda (x) (org-ref-get-bibtex-entry-md x)) keys "\n\n")
         "\n")))))

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

  ;; The following code implements my way of Zettelkasten instead of using
  ;; org-roam which is too slow.

  ;; Modified from `org-refile-get-location'
  ;; Create new nodes by capture when necessary.
  (defun zenith/org-refile-get-location (&optional prompt default-buffer new-nodes)
    "Prompt the user for a refile location, using PROMPT.
PROMPT should not be suffixed with a colon and a space, because
this function appends the default value from
`org-refile-history' automatically, if that is not empty."
    (let ((org-refile-targets org-refile-targets)
          (org-refile-use-outline-path org-refile-use-outline-path))
      (setq org-refile-target-table (org-refile-get-targets default-buffer)))
    (unless org-refile-target-table
      (user-error "No refile targets"))
    (let* ((cbuf (current-buffer))
           (cfn (buffer-file-name (buffer-base-buffer cbuf)))
           (cfunc (if (and org-refile-use-outline-path
                           org-outline-path-complete-in-steps)
                      #'org-olpath-completing-read
                    #'completing-read))
           (extra (if org-refile-use-outline-path "/" ""))
           (cbnex (concat (buffer-name) extra))
           (filename (and cfn (expand-file-name cfn)))
           (tbl (mapcar
                 (lambda (x)
                   (if (and (not (member org-refile-use-outline-path
                                         '(file full-file-path)))
                            (not (equal filename (nth 1 x))))
                       (cons (concat (car x) extra " ("
                                     (file-name-nondirectory (nth 1 x)) ")")
                             (cdr x))
                     (cons (concat (car x) extra) (cdr x))))
                 org-refile-target-table))
           (completion-ignore-case t)
           cdef
           (prompt (concat prompt
                           (or (and (car org-refile-history)
                                    (concat " (default " (car org-refile-history) ")"))
                               (and (assoc cbnex tbl) (setq cdef cbnex)
                                    (concat " (default " cbnex ")"))) ": "))
           pa answ parent-target child parent old-hist)
      (setq old-hist org-refile-history)
      (setq answ (funcall cfunc prompt tbl nil (not new-nodes)
                          nil 'org-refile-history
                          (or cdef (car org-refile-history))))
      (if (setq pa (org-refile--get-location answ tbl))
          (let ((last-refile-loc (car org-refile-history)))
            (org-refile-check-position pa)
            (when (or (not org-refile-history)
                      (not (eq old-hist org-refile-history))
                      (not (equal (car pa) last-refile-loc)))
              (setq org-refile-history
                    (cons (car pa) (if (assoc last-refile-loc tbl)
                                       org-refile-history
                                     (cdr org-refile-history))))
              (when (equal last-refile-loc (nth 1 org-refile-history))
                (pop org-refile-history)))
            pa)
        (if (string-match "\\`\\(.*\\)\\'" answ)
            (progn (setq title (match-string 1 answ))
                   (when
                       (or (eq new-nodes t)
                           (and (eq new-nodes 'confirm)
                                (y-or-n-p (format "Create new node \"%s\"? "
                                                  child))))
                     (let (
                           (org-capture-templates
                            (list (zenith/convert-template (car org-capture-templates)
                                                           '(:unnarrowed t :immediate-finish t) title))))
                       (org-capture nil (caar org-capture-templates))
                       org-capture-last-stored-marker)))
          (user-error "Invalid target location")))))

  ;; Modified from `org-id-get-with-outline-path-completion'
  (defun zenith/org-id-get-with-outline-path-completion (&optional targets)
    "Use `outline-path-completion' to retrieve the ID of an entry.
TARGETS may be a setting for `org-refile-targets' to define
eligible headlines.  When omitted, all headlines in the current
file are eligible.  This function returns the ID of the entry.
If necessary, the ID is created."
    (let* ((org-refile-targets (or targets '((nil . (:maxlevel . 10)))))
           (org-refile-use-outline-path
            (if (caar org-refile-targets) 'file t))
           (org-refile-target-verify-function nil)
           (spos (zenith/org-refile-get-location "Entry" nil t))
           (pom (and spos (if (listp spos)
                              (move-marker (make-marker) (or (nth 3 spos) 1)
                                           (find-buffer-visiting (nth 1 spos)))
                            (copy-marker spos)))))
      (prog1 (org-id-get pom 'create)
        (move-marker pom nil))))

  (defun org-id-complete-link (&optional arg)
    "Create an id: link using completion"
    (concat "id:"
            (zenith/org-id-get-with-outline-path-completion org-refile-targets)))
  (org-link-set-parameters "id"
                           :complete 'org-id-complete-link)

  ;; Always respect behavior of `org-link-frame-setup', regardless of opening
  ;; the same file or not.
  (defun zenith/org-id-open (id _)
    "Go to the entry with id ID."
    (org-mark-ring-push)
    (let ((m (org-id-find id 'marker))
          cmd)
      (unless m
        (error "Cannot find entry with ID \"%s\"" id))
      ;; Use a buffer-switching command in analogy to finding files
      (setq cmd
            (or
             (cdr
              (assq
               (cdr (assq 'file org-link-frame-setup))
               '((find-file . switch-to-buffer)
                 (find-file-other-window . switch-to-buffer-other-window)
                 (find-file-other-frame . switch-to-buffer-other-frame))))
             'switch-to-buffer-other-window))
      (funcall cmd (marker-buffer m))
      (goto-char m)
      (move-marker m nil)
      (org-show-context)))
  (org-link-set-parameters "id"
                           :follow 'zenith/org-id-open)

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

  (defun zenith/org-insert-link (&optional arg)
    "Insert the stored link or by id"
    (interactive "p")
    (if org-stored-links
        (org-insert-last-stored-link 1)
      (let ((org-refile-targets
             (pcase arg
               (4 org-refile-targets)
               (_ '((nil :maxlevel . 4))))))
        (zenith/org-insert-link-by-id)))))

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

  ;; To avoid cleaning multiple timestamp element, the timestamps should be put in reverse order.
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
      (max (/ (- (max 7 interval) time) interval) -1)))

  (defun -zenith/org-clone-repeats ()
    "Repeat the daily task 7 times"
    (when-let* ((org-trust-scanner-tags t)
                (timestamp (org-entry-get nil "TIMESTAMP"))
                (rep (car-safe (s-match "\\+[0-9]+[dwmy]" timestamp)))
                (interval (zenith/org-calc-interval rep))
                (times (zenith/org-calc-clones timestamp interval))
                (check (>= times 0)))
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
    (zenith/org-clone-repeats)
    (zenith/org-clean-agenda))

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

  (defun zenith/org-agenda-grid-tweakify (orig-fun list ndays todayp)
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
          (funcall orig-fun list ndays todayp))
      (funcall orig-fun list ndays todayp)))
  (advice-add 'org-agenda-add-time-grid-maybe :around 'zenith/org-agenda-grid-tweakify)

  ;; (add-hook 'org-agenda-finalize-hook 'org-icalendar-combine-agenda-files)
  ;; The evalution process is added to the crontab to run every one hour

  (defun zenith/org-review-note-today (&optional arg)
    (interactive "p")
    (let* ((org-agenda-files (list zenith/note-directory))
           (org-export-use-babel nil)
           (org-agenda-finalize-hook nil)
           (time (pcase arg
                   (4 "-1w")
                   (16 "-1m")
                   (_ "today")))
           (match (concat "TIMESTAMP_IA>=\"<" time ">\"")))
      (org-tags-view nil match)))

  ;; Adpated from
  ;; https://org-roam.discourse.group/t/export-backlinks-on-org-export/1756/21
  (defun collect-backlinks-string ()
    (interactive)
    (org-show-all)
    (org-roam-update-org-id-locations)
    (org-roam-db-sync)
    (when-let* (
                (source-file buffer-file-name)
                (check (org-roam-file-p source-file))
                (nodes-in-file (--filter (s-equals? (org-roam-node-file it) source-file)
                                         (org-roam-node-list)))
                (nodes-start-position (-map 'org-roam-node-point nodes-in-file))
                ;; Nodes don't store the last position, so get the next headline position
                ;; and subtract one character (or, if no next headline, get point-max)
                ;; (nodes-end-position (-map (lambda (nodes-start-position)
                ;;                             (goto-char nodes-start-position)
                ;;                             (if (org-before-first-heading-p) ;; file node
                ;;                                 (point-max)
                ;;                               (call-interactively
                ;;                                'org-next-visible-heading)
                ;;                               (if (> (point) nodes-start-position)
                ;;                                   (- (point) 1) ;; successfully found next
                ;;                                 (point-max)))) ;; there was no next
                ;;                           nodes-start-position))
                ;; sort in order of decreasing end position
                (nodes-in-file-sorted (->> (-zip nodes-in-file nodes-start-position)
                                           (--sort (> (cdr it) (cdr other))))))
      (dolist (node-and-start nodes-in-file-sorted)
        (-let (((node . start-position) node-and-start)
               (heading "REFERENCED")
               (values))
          (goto-char start-position)
          (if (org-roam-backlinks-get node)
              (progn
                ;; Add the references as a subtree of the node
                ;; (setq heading "\n\nReferences: ")
                ;; (insert heading)
                (with-temp-buffer
                  (let ((backlinks (cl-remove-duplicates
                                    (-map 'org-roam-backlink-source-node
                                          (org-roam-backlinks-get node)) :key 'org-roam-node-id :test 'string-equal)))
                    (dolist (backlink backlinks)
                      (let* ((reference (format "[[id:%s][%s]], "
                                                (org-roam-node-id backlink)
                                                (org-roam-node-title backlink))))
                        (insert reference)))
                    (delete-backward-char 2)
                    (setq values (buffer-substring-no-properties (point-min) (point-max)))))
                (org-set-property heading values))
            (org-delete-property heading)))))))

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
        org-roam-completion-everywhere nil
        org-roam-node-display-template "${my-olp}${title} ${my-file}")

  ;; Don't include Review.org as nodes
  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "Review" (org-get-tags)))))
  ;; I change the way org-roam-capture- work to fit my headline oriented
  ;; workflow. However, I found that org-roam is extremely slow of no particular
  ;; reason, so I invent my new way.
  (cl-defmethod org-roam-node-my-olp ((node org-roam-node))
    (let ((olp (org-roam-node-olp node)))
      (if olp
          (concat (s-join "/" (org-roam-node-olp node)) "/")
        "")))

  (cl-defmethod org-roam-node-my-file ((node org-roam-node))
    (let ((file (org-roam-node-file node)))
      (concat "(" (file-name-nondirectory file) ")")))

  (defun zenith/convert-template (template props title)
    "Modify the template for org-roam."
    (let ((templ (cl-copy-list template))
          (expansion (nth 4 template))
          org-roam-plist options)
      (while props
        (let* ((key (pop props))
               (val (pop props))
               (custom (member key org-roam-capture--template-keywords)))
          (if custom
              (setq org-roam-plist (plist-put org-roam-plist key val))
            (setq options (plist-put options key val)))))
      (setcar (nthcdr 4 templ) (replace-regexp-in-string "%^{Title}" title expansion t t))
      (append templ options (list :org-roam org-roam-plist))))

  ;; Redefine org-roam-capture to integrate my own org-roam workflow
  (cl-defun org-roam-capture- (&key goto keys node info props templates)
    "Personal version of org-roam-capture-"
    (let* ((props (plist-put props :call-location (point-marker)))
           (title (org-roam-node-title node))
           (org-capture-templates (list (zenith/convert-template (car org-capture-templates) props title)))
           (org-roam-capture--node node)
           (org-roam-capture--info info))

      (when (not keys)
        (setq keys (caar org-capture-templates)))
      (org-capture goto keys)))

  (defun org-roam-open-id-at-point-advice (orig-fun)
    nil)
  (advice-add 'org-roam-open-id-at-point :around 'org-roam-open-id-at-point-advice))

;; Global settings
(defun zenith/my-org-agenda ()
  (interactive)
  (org-agenda 0 "b"))

(with-eval-after-load 'calfw-org
  (setq cfw:org-overwrite-default-keybinding t))

(provide 'init-org)
;;; init-org.el ends here
