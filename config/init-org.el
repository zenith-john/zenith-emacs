;;; init-org.el --- org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Majority of the file from
;;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/*.el
;; Changed org defaults (should be set before org loads)

;; Code:

;; org-mode
(defvar org-directory "~/Dropbox/")
(defvar org-agenda-files '("~/Dropbox/"))
(defvar org-modules
  '(
    ;;org-w3m
    ;; org-bbdb
    org-bibtex
    org-docview
    ;; org-gnus
    org-info
    ;; org-irc
    ;; org-mhe
    ;; org-rmail
    ))


;;
;;; Packages

(zenith/autoload '(org-clock-load org-clock-save) "org-clock")
(add-hook 'org-mode-hook 'org-clock-load)
(setq org-clock-persist 'history
      org-clock-persist-file (concat zenith-emacs-local-dir "org-clock-save.el"))
(add-hook 'kill-emacs-hook #'org-clock-save)

(defun org/setup-hook ()
  "Configures the UI for `org-mode'."
  (setq-default
   org-adapt-indentation nil
   org-blank-before-new-entry '((heading . nil) (plain-list-item . nil))
   org-cycle-include-plain-lists t
   org-catch-invisible-edits 'show-and-error
   org-eldoc-breadcrumb-separator " → "
   org-entities-user
   '(("flat"  "\\flat" nil "" "" "266D" "♭")
     ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hidden-keywords nil
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-indent-mode-turns-on-hiding-stars t
   org-insert-heading-respect-content t
   org-list-description-max-indent 4
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-priority-faces
   '((?a . error)
     (?b . warning)
     (?c . success))
   org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3))
   org-special-ctrl-a/e t
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w@/!)" "PAUSE(p)" "SOMEDAY(s)" "NEXT(n)" "|" "DONE(d!)" "CANCELLED(c@)")
     (sequence "[ ](T)" "[-](P)" "[?](m)" "|" "[X](D)"))
   org-todo-keyword-faces
   '(("[-]" :inherit (font-lock-constant-face bold))
     ("[?]" :inherit (warning bold))
     ("WAITING" :inherit bold)
     ("LATER" :inherit (warning bold)))
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

  ;; Make emphasis clear when using bold font
  (add-to-list 'org-emphasis-alist
               '("*" (:foreground "pink"))))


;;
;;; Bootstrap
(defun zenith/org-mode-hook ()
  (org-indent-mode)
  (auto-fill-mode)
  ;; cdlatex
  ;; Enable cdlatex mode
  ;; TODO configure cdlatex-command-alist
  (setq-local company-idle-delay nil)
  (display-line-numbers-mode 0)
  (org-cdlatex-mode 1)
  (LaTeX-math-mode 1)
  ;; org-edit-latex
  ;; dependencies: auctex
  (require 'org-edit-latex)
  (org-edit-latex-mode 1)
  ;; ox-hugo
  ;; dependencies: org
  (require 'ox-hugo)
  ;; org-bullet
  (require 'org-bullets)
  (org-bullets-mode)
  ;; org-ref
  (require 'org-ref)
  )

(add-hook 'org-mode-hook 'org-setup-hook)
(add-hook 'org-mode-hook 'zenith/org-mode-hook)

(setq org-capture-templates
      '(
        ("h" "Homework" entry (file+headline "~/Dropbox/task.org"  "Homework")
         "* TODO %? :Homework:\n")
        ("s" "Schedule" entry (file+headline "~/Dropbox/task.org" "Schedule")
         "* %?\n")
        ("r" "Project" entry (file+headline "~/Dropbox/task.org" "Project")
         "* TODO %?\n")
        ("q" "Question" entry (file+headline "~/Dropbox/task.org" "Question")
         "* TODO %? :Question:\n")
        ("d" "Idea" entry (file+headline "~/Dropbox/task.org" "Idea")
         "* TODO %? :Idea:\n")))

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

;; Org agenda settings
(setq org-enable-table-editor 'optimized
      org-agenda-start-on-weekday nil
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-span 7
      org-agenda-compact-blocks t
      org-agenda-show-all-dates nil
      org-deadline-warning-days 365
      org-agenda-show-future-repeats t
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

;; Org attach
(require 'org-attach)
(setq org-attach-method 'lns)

;; Org-agenda export to icalendar
(require 'ox-icalendar)
(setq org-icalendar-combined-agenda-file (expand-file-name "~/Dropbox/agenda.ics")
      org-icalendar-include-todo t
      org-icalendar-use-deadline '(event-if-not-todo todo-due)
      org-icalendar-use-scheduled '(event-if-not-todo todo-start)
      org-icalendar-alarm-time 15
      org-icalendar-store-UID t
      org-agenda-default-appointment-duration 90)

(add-hook 'org-agenda-finalize-hook 'org-icalendar-combine-agenda-files)

;; I prefer C-c C-c over C-c ' (more consistent)
(define-key org-src-mode-map (kbd "C-c C-c") #'org-edit-src-exit)

(eval-after-load 'ox-latex
  '(add-to-list 'org-latex-classes
    '("ctexart"
      "\\documentclass{ctexart}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(defun zenith/my-org-agenda ()
  (interactive)
  (org-agenda 0 "b"))

(general-define-key
 :keymaps 'normal
 :prefix ","
 "a" 'org-agenda
 "A" 'zenith/my-org-agenda)

;; org-ref
;; dependencies: htmlize helm helm-bibtex ivy hydra key-chord s f pdf-tools
(setq org-ref-completion-library 'org-ref-ivy-cite)

(setq
 org-ref-prefer-bracket-links t
 org-ref-pdf-directory (expand-file-name "~/Documents/Library/")
 org-ref-default-bibliography `( ,(expand-file-name "~/Dropbox/Library.bib")))

;; Make citation work
(setq org-latex-pdf-process
      '("%latex -interaction nonstopmode -output-directory %o %f"
        "biber %b"
        "%latex -interaction nonstopmode -output-directory %o %f"
        "%latex -interaction nonstopmode -output-directory %o %f"))

(require 'evil-org)
(require 'evil-org-agenda)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme)
(evil-org-agenda-set-keys)
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
(evil-define-key 'motion org-agenda-mode-map
  "a" 'org-attach
  "o" 'org-agenda-attach-open)

(provide 'init-org)
;;; init-org.el ends here
