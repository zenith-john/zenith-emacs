;;; init-latex.el --- AucTex configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(defvar +latex-company-backends '())

(use-package ivy-completion
  :commands (ivy-bibtex)
  :after ivy
  :init
  (add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-ignore-order))
  (setq bibtex-completion-bibliography (expand-file-name "~/Dropbox/Library.bib")
        bibtex-completion-additional-search-fields '("abstract")))

(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :init
  (add-to-list '+latex-company-backends 'company-reftex-labels)
  (add-to-list '+latex-company-backends 'company-reftex-citations)
  :config
  ;; Get ReTeX working with biblatex
  ;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
  (setq reftex-cite-format
        '((?a . "\\autocite[]{%l}")
          (?b . "\\blockcquote[]{%l}{}")
          (?c . "\\cite[]{%l}")
          (?f . "\\footcite[]{%l}")
          (?n . "\\nocite{%l}")
          (?p . "\\parencite[]{%l}")
          (?s . "\\smartcite[]{%l}")
          (?t . "\\textcite[]{%l}"))
        reftex-plug-into-AUCTeX t
        reftex-toc-split-windows-fraction 0.3)
  (add-hook 'reftex-toc-mode-hook
    (lambda () (reftex-toc-rescan))))

;; set up mode for bib files
(with-eval-after-load 'bibtex
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20))

(use-package company-auctex
  :defer t
  :init
  (add-to-list '+latex-company-backends 'company-auctex-environments)
  (add-to-list '+latex-company-backends 'company-auctex-macros))

(use-package company-math
  :defer t
  :init
  (add-to-list '+latex-company-backends 'company-math-symbols-latex)
  (add-to-list '+latex-company-backends 'company-math-symbols-unicode)
  (add-to-list '+latex-company-backends 'company-latex-commands))


(add-to-list 'auto-mode-alist '("\\.tex\\'" . TeX-latex-mode))


(with-eval-after-load 'tex
  (setq TeX-parse-self t ; parse on load
        TeX-auto-save t  ; parse on save
        ;; use hidden dirs for auctex files
        TeX-auto-local ".auctex-auto"
        TeX-style-local ".auctex-style"
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        ;; don't start the emacs server when correlating sources
        TeX-source-correlate-start-server nil
        ;; automatically insert braces after sub/superscript in math mode
        TeX-electric-sub-and-superscript t)
  ;; fontify common latex commands
  ;; Fontification taken from https://tex.stackexchange.com/a/86119/81279
  (setq font-latex-match-reference-keywords
        '(;; biblatex
          ("printbibliography" "[{")
          ("addbibresource" "[{")
          ;; Standard commands
          ("cite" "[{")
          ("citep" "[{")
          ("citet" "[{")
          ("Cite" "[{")
          ("parencite" "[{")
          ("Parencite" "[{")
          ("footcite" "[{")
          ("footcitetext" "[{")
          ;; Style-specific commands
          ("textcite" "[{")
          ("Textcite" "[{")
          ("smartcite" "[{")
          ("Smartcite" "[{")
          ("cite*" "[{")
          ("parencite*" "[{")
          ("supercite" "[{")
          ;; Qualified citation lists
          ("cites" "[{")
          ("Cites" "[{")
          ("parencites" "[{")
          ("Parencites" "[{")
          ("footcites" "[{")
          ("footcitetexts" "[{")
          ("smartcites" "[{")
          ("Smartcites" "[{")
          ("textcites" "[{")
          ("Textcites" "[{")
          ("supercites" "[{")
          ;; Style-independent commands
          ("autocite" "[{")
          ("Autocite" "[{")
          ("autocite*" "[{")
          ("Autocite*" "[{")
          ("autocites" "[{")
          ("Autocites" "[{")
          ;; Text commands
          ("citeauthor" "[{")
          ("Citeauthor" "[{")
          ("citetitle" "[{")
          ("citetitle*" "[{")
          ("citeyear" "[{")
          ("citedate" "[{")
          ("citeurl" "[{")
          ;; Special commands
          ("fullcite" "[{")
          ;; cleveref
          ("cref" "{")
          ("Cref" "{")
          ("cpageref" "{")
          ("Cpageref" "{")
          ("cpagerefrange" "{")
          ("Cpagerefrange" "{")
          ("crefrange" "{")
          ("Crefrange" "{")
          ("labelcref" "{")))

  (setq font-latex-match-textual-keywords
        '(;; biblatex brackets
          ("parentext" "{")
          ("brackettext" "{")
          ("hybridblockquote" "[{")
          ;; Auxiliary Commands
          ("textelp" "{")
          ("textelp*" "{")
          ("textins" "{")
          ("textins*" "{")
          ;; subcaption
          ("subcaption" "[{")))

  (setq font-latex-match-variable-keywords
        '(;; amsmath
          ("numberwithin" "{")
          ;; enumitem
          ("setlist" "[{")
          ("setlist*" "[{")
          ("newlist" "{")
          ("renewlist" "{")
          ("setlistdepth" "{")
          ("restartlist" "{")
          ("crefname" "{")))
  ;; prompt for master
  (setq-default TeX-master nil)
  ;; set-up chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  ;; tell emacs how to parse tex files
  (add-hook 'TeX-mode-hook (lambda () (setq ispell-parser 'tex)))
  ;; Enable word wrapping
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  ;; Add company-backends
  (add-hook 'TeX-mode-hook (lambda ()
                             (make-local-variable company-backends)
                             (add-to-list 'company-backends +latex-company-backends)))
  ;; Fold TeX macros
  (add-hook 'TeX-mode-hook #'TeX-fold-mode)
  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-mode-hook #'rainbow-delimiters-mode)
  ;; Do not prompt for Master files, this allows auto-insert to add templates to
  ;; .tex files
  (add-hook 'TeX-mode-hook
    ;; Necessary because it is added as an anonymous, byte-compiled function
    (remove-hook 'find-file-hook
                 (cl-find-if #'byte-code-function-p find-file-hook)
                 'local))
  (add-hook 'latex-mode-local-vars-hook #'flyspell-mode)
  ;; All these excess pairs dramatically slow down typing in latex buffers, so
  ;; we remove them. Let snippets do their job.
  (with-eval-after-load 'smartparens-latex
    (let ((modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)))
      (dolist (open '("\\left(" "\\left[" "\\left\\{" "\\left|"
                      "\\bigl(" "\\biggl(" "\\Bigl(" "\\Biggl(" "\\bigl["
                      "\\biggl[" "\\Bigl[" "\\Biggl[" "\\bigl\\{" "\\biggl\\{"
                      "\\Bigl\\{" "\\Biggl\\{"
                      "\\lfloor" "\\lceil" "\\langle"
                      "\\lVert" "\\lvert" "`"))
        (sp-local-pair modes open nil :actions :rem))
      (sp-local-pair modes "``" nil :unless '(:add sp-in-math-p)))))


(with-eval-after-load 'latex
  (setq LaTeX-section-hook ; Add the toc entry to the sectioning hooks.
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-toc
          LaTeX-section-section
          LaTeX-section-label)
        LaTeX-fill-break-at-separators nil
        LaTeX-item-indent 0))


(provide 'init-latex)
;;; init-latex.el ends here
