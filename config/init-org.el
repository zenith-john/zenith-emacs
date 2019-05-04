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
  (require 'ox-hugo))

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

(provide 'init-org)
;;; init-org.el ends here
