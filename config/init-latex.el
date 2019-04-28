;;; init-latex.el --- AucTex configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(use-package ivy-completion
  :commands (ivy-bibtex)
  :after ivy
  :init
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-ignore-order))
  (setq bibtex-completion-bibliography (expand-file-name "~/Dropbox/Library.bib")
        bibtex-completion-additional-search-fields '("abstract")))

(provide 'init-latex)
;;; init-latex.el ends here
