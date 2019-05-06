;;; init-org.el --- org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
;;;
(use-package org-edit-latex
  :after org
  :init
  (require 'org-edit-latex))

(use-package ox-hugo
  :after ox
  :init
  (require 'ox-hugo)
  (add-to-list 'org-hugo-langs-no-descr-in-code-fences 'nil))

(use-package org-noter
  :commands (org-noter)
  :after pdf-tools
  :config
  ;; Overriding the function due to make visual-line mode no effect.
  (defun org-noter--set-notes-scroll (window &rest ignored)
    nil)
  (setq org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil
        org-noter-hide-other nil)
  ;; Define keymap for org-noter-doc for one-key quit
  (defun zenith/org-noter-doc-hook ()
    (general-nmap
      :keymaps 'local
      "C-i" 'org-noter-insert-note-toggle-no-questions
      "q" 'org-noter-kill-session
      "i" 'org-noter-insert-note))
  (add-hook 'org-noter-doc-mode-hook 'zenith/org-noter-doc-hook))

(use-package org-ref
  :after (org)
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq org-ref-cite-onclick-function (lambda (_) (org-ref-cite-hydra/body))
        org-ref-pdf-directory (expand-file-name "~/Documents/Library/")
        org-ref-default-bibliography `( ,(expand-file-name "~/Dropbox/Library.bib")))

  ;; Make citation work
  (setq org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "biber %b"
          "%latex -interaction nonstopmode -output-directory %o %f"
          "%latex -interaction nonstopmode -output-directory %o %f")))

;; Reconfigure org
(with-eval-after-load 'org

  (add-hook 'org-mode-hook (lambda ()
    ;; Enable cdlatex mode
    ;; TODO configure cdlatex-command-alist
    (setq-local company-idle-delay nil)
    (flycheck-mode 0)
    (visual-line-mode 1)
    (display-line-numbers-mode 0)
    (org-cdlatex-mode 1)
    (LaTeX-math-mode 1)
    (setq truncate-lines nil)
    (sp-with-modes 'org-mode
      (sp-local-pair "=" "=" :actions :rem))))

  (remove-hook 'org-mode-hook
    #'auto-fill-mode)

  ;; Make emphasis clear when using bold font
  (add-to-list 'org-emphasis-alist
               '("*" (:foreground "pink")))

  (setf org-highlight-latex-and-related '(latex))

  ;; Make latex formulation more clear
  (plist-put org-format-latex-options :scale 2)

  (require 'org-edit-latex)

  (setq org-directory "~/Dropbox/"
        org-agenda-files '("~/Dropbox/")
        org-default-notes-file "~/Dropbox/refile.org")

  ;; Org-todo and org-capture
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "PAUSE(p)" "SOMEDAY(s)" "NEXT(n)" "|" "DONE(d!)" "CANCELLED(c@)")
          (sequence "[ ](T)" "[-](P)" "[?](m)" "|" "[X](D)")))

  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("NEXT" ("WAITING" . nil) ("SOMEDAY" . nil))
          (done ("WAITING" . nil))
          ("PAUSE" ("WAITING" . nil) ("CANCELLED" . nil))
          ("TODO" ("WAITING" . nil) ("CANCELLED" . nil) ("SOMEDAY" . nil))
          ("DONE" ("WAITING" . nil) ("TODO" . nil) ("SOMEDAY" . nil))
          ("SOMEDAY" ("TODO" . nil))))

  ;; Org Capture
  (defun transform-square-brackets-to-round-ones(string-to-transform)
    "Transforms [ into ( and ] into ), other chars left unchanged."
    (concat
     (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform)))

  (setq org-capture-templates
        '(("p" "Protocol" entry (file+headline "~/Documents/Notes/notes.org" "Bookmarks")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	      ("L" "Protocol Link" entry (file+headline "~/Documents/Notes/notes.org" "Bookmarks")
           "* %? [[%:link][%(transform-square-brackets-to-round-ones \"%:description\")]]\n")
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
  (defvar kk/delete-frame-after-capture 0 "Whether to delete the last frame after the current capture")

  (defun kk/delete-frame-if-neccessary (&rest r)
    (cond
     ((= kk/delete-frame-after-capture 0) nil)
     ((> kk/delete-frame-after-capture 1)
      (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
     (t
      (setq kk/delete-frame-after-capture 0)
      (delete-frame))))

  (advice-add 'org-capture-finalize :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-kill :after 'kk/delete-frame-if-neccessary)
  (advice-add 'org-capture-refile :after 'kk/delete-frame-if-neccessary)

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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (python . t)
     (shell . t)
     (dot . t)))

  (eval-after-load 'ox-latex
    '(add-to-list 'org-latex-classes
                  '("ctexart"
                    "\\documentclass{ctexart}"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\subsection{%s}" . "\\subsection*{%s}")
                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

(provide 'init-org)
;;; init-org.el ends here
