;;; init-org.el --- org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Majority of the file from
;;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/org/*.el
;; Changed org defaults (should be set before org loads)

;; Code:
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

(use-package org-crypt ; built-in
  :commands org-encrypt-entries
  :hook (org-reveal-start . org-decrypt-entry)
  :init
  (add-hook 'org-mode-hook
            (lambda ()(add-hook 'before-save-hook 'org-encrypt-entries nil t)))
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address))

(use-package org-clock ; built-in
  :commands org-clock-save
  :hook (org-mode . org-clock-load)
  :init
  (setq org-clock-persist 'history
        org-clock-persist-file (concat zenith-emacs-local-dir "org-clock-save.el"))
  :config
  (add-hook 'kill-emacs-hook #'org-clock-save))


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
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5)

   org-todo-state-tags-triggers
   '(("CANCELLED" ("CANCELLED" . t))
     ("WAITING" ("WAITING" . t))
     ("NEXT" ("WAITING" . nil) ("SOMEDAY" . nil))
     (done ("WAITING" . nil))
     ("PAUSE" ("WAITING" . nil) ("CANCELLED" . nil))
     ("TODO" ("WAITING" . nil) ("CANCELLED" . nil) ("SOMEDAY" . nil))
     ("DONE" ("WAITING" . nil) ("TODO" . nil) ("SOMEDAY" . nil))
     ("SOMEDAY" ("TODO" . nil))))

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
(use-package org
  :init
  (general-add-hook 'org-mode-hook
                    #'(
                       org-indent-mode            ; margin-based indentation
                       auto-fill-mode             ; line wrapping
                       ))

  (add-hook 'org-mode-hook (lambda ()
                             ;; Enable cdlatex mode
                             ;; TODO configure cdlatex-command-alist
                             (setq-local company-idle-delay nil)
                             (display-line-numbers-mode 0)
                             (org-cdlatex-mode 1)
                             (LaTeX-math-mode 1)))

  (add-hook 'org-mode-hook (lambda ()
                             (require 'org-edit-latex)
                             (org-edit-latex-mode 1)))

  (add-hook 'org-mode-hook (lambda ()
                             (require 'ox-hugo)
                             (add-to-list 'org-hugo-langs-no-descr-in-code-fences 'nil)))
  :config
  (org/setup-hook)

  ;; Org Capture

  ;; Enable org protocol
  ;; (require 'org-protocol)

  ;; (defun transform-square-brackets-to-round-ones(string-to-transform)
  ;;   "Transforms [ into ( and ] into ), other chars left unchanged."
  ;;   (concat
  ;;    (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

  (setq org-capture-templates
        '(
          ;; ("p" "Protocol" entry (file+headline "~/Documents/Notes/notes.org" "Bookmarks")
          ;;  "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ;; ("L" "Protocol Link" entry (file+headline "~/Documents/Notes/notes.org" "Bookmarks")
          ;;  "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
          ("h" "Homework" entry (file+headline "~/Dropbox/task.org"  "Homework")
           "* TODO %? :Homework:\n")
          ("s" "Schedule" entry (file+headline "~/Dropbox/task.org" "Schedule")
           "* TODO %?\n")
          ("r" "Project" entry (file+headline "~/Dropbox/task.org" "Project")
           "* TODO %?\n")
          ("q" "Question" entry (file+headline "~/Dropbox/task.org" "Question")
           "* TODO %? :Question:\n")
          ("d" "Idea" entry (file+headline "~/Dropbox/task.org" "Idea")
           "* TODO %? :Idea:\n")))

  ;; For org-protocol from https://github.com/sprig/org-capture-extension
  ;; (defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

  ;; (defun kk/delete-frame-if-neccessary (&rest r)
  ;;   (cond
  ;;    ((= kk/delete-frame-after-capture 0) nil)
  ;;    ((> kk/delete-frame-after-capture 1)
  ;;     (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
  ;;    (t
  ;;     (setq kk/delete-frame-after-capture 0)
  ;;     (delete-frame))))

  ;; (advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
  ;; (advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
  ;; (advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)

  ;; Org tag
  (setq org-tag-alist
        '(("Improvement" . ?i) ("Homework" . ?h) ("Personal" . ?p) ("Question" . ?q) ("Idea" . ?d)))


  ;; Org agenda settings
  (setq org-enable-table-editor 'optimized
        org-agenda-start-on-weekday nil
        org-agenda-skip-scheduled-if-deadline-is-shown t
        org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled)
        org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-span 14
        org-agenda-compact-blocks t
        org-agenda-show-all-dates nil
        org-deadline-warning-days 365
        org-agenda-show-future-repeats nil)


  (setq org-agenda-custom-commands
        '(("b" "My Agenda View" ((tags-todo "Question/!-Waiting/!-Pause"
                                            ((org-agenda-overriding-header "Unsolved Questions:")))
                                 (tags "AGENDAHEADER" ((org-agenda-overriding-header "========================================\nToday's Schedule:")))
                                 (agenda ""
                                         ((org-agenda-show-all-dates t)
                                          (org-agenda-span 'day)
                                          (org-deadline-warning-days 0)
                                          (org-agenda-start-day "+0d")))
                                 (todo "NEXT"
                                       ((org-agenda-overriding-header "========================================\nNext Tasks:")))
                                 (tags "BEFOREWEEKGLANCE" ((org-agenda-overriding-header "========================================\nNext Week Glance:")))
                                 (agenda ""
                                         ((org-agenda-show-all-dates t)
                                          (org-agenda-span 6)
                                          (org-agenda-start-day "+1d")))
                                 (tags-todo "Improvement/!-NEXT" ((org-agenda-overriding-header "========================================\nImprove Yourself:")))
                                 (tags-todo "Idea+TODO<>\"NEXT\"|Personal+TODO<>\"NEXT\"" ((org-agenda-overriding-header "\nPersonal Project:")))
                                 (tags "BEFOREDEADLINE" ((org-agenda-overriding-header "========================================\nFar Away Tasks:")))
                                 (agenda ""
                                         ((org-agenda-span 180)
                                          (org-agenda-time-grid nil)
                                          (org-agenda-show-all-dates nil)
                                          (org-agenda-entry-types '(:deadline :scheduled))
                                          (org-agenda-start-day "+7d")))))))

  ;; Org latex export
  (setq org-latex-with-hyperref t)
  (setq org-latex-compiler "xelatex")
  (setq org-latex-default-packages-alist
        '(("" "hyperref" nil)
          ("AUTO" "inputenc" t)
          ("" "fixltx2e" nil)
          ("" "graphicx" t)
          ("" "longtable" nil)
          ("" "float" nil)
          ("" "wrapfig" nil)
          ("" "rotating" nil)
          ("normalem" "ulem" t)
          ("" "amsmath" t)
          ("" "textcomp" t)
          ("" "marvosym" t)
          ("" "wasysym" t)
          ("" "multicol" t)
          ("" "amssymb" t)
          "\\tolerance=1000"))

  (require 'ob-async)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (shell . t)
     (dot . t)))

  (setq org-src-fontify-natively t      ; make code pretty
        org-src-preserve-indentation t  ; use native major-mode indentation
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate nil)

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

  (general-define-key "C-c C-a" #'zenith/my-org-agenda))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; (use-package org-noter
;;   :commands (org-noter)
;;   :after pdf-tools
;;   :config
;;   ;; Overriding the function due to make visual-line mode no effect.
;;   (defun org-noter--set-notes-scroll (window &rest ignored)
;;     nil)
;;   (setq org-noter-always-create-frame nil
;;         org-noter-kill-frame-at-session-end nil
;;         org-noter-hide-other nil)
;;   ;; Define keymap for org-noter-doc for one-key quit
;;   (defun zenith/org-noter-doc-hook ()
;;     (general-def
;;       pdf-view-mode-map
;;       "C-i" 'org-noter-insert-note-toggle-no-questions
;;       "q" 'org-noter-kill-session
;;       "i" 'org-noter-insert-note))
;;   (defun zenith/org-noter-notes-hook ()
;;     (ws-butler-mode -1))
;;   (add-hook 'org-noter-doc-mode-hook #'zenith/org-noter-doc-hook)
;;   (add-hook 'org-noter-notes-mode-hook #'zenith/org-noter-notes-hook))

(use-package org-ref
  :after (org)
  :defer 2
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq
   org-ref-prefer-bracket-links t
   org-ref-pdf-directory (expand-file-name "~/Documents/Library/")
   org-ref-default-bibliography `( ,(expand-file-name "~/Dropbox/Library.bib")))

  ;; Make citation work
  (setq org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "biber %b"
          "%latex -interaction nonstopmode -output-directory %o %f"
          "%latex -interaction nonstopmode -output-directory %o %f")))

(provide 'init-org)
;;; init-org.el ends here
