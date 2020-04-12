;;; init-latex.el --- AucTex configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(defvar +latex-company-backends '())
(defvar zenith/bibtex-library (expand-file-name "~/Dropbox/Library.bib")
  "The default bibtex library")

;; helm-bibtex
;; dependencies: swiper parsebib s dash f biblio
(autoload 'ivy-bibtex "ivy-bibtex" "" t)
(add-to-list 'ivy-re-builders-alist '(ivy-bibtex . ivy--regex-ignore-order))
(setq bibtex-completion-bibliography zenith/bibtex-library
      bibtex-completion-additional-search-fields '("abstract"))

;; ebib
;; depedencies: parsebib
(require 'ebib)
(setq ebib-preload-bib-files `(,zenith/bibtex-library))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
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
      reftex-toc-split-windows-fraction 0.3
	  reftex-bibpath-environment-variables '("/home/zenith-john/Dropbox/")
	  reftex-bibliography-commands '("bibliography" "nobibiliography" "addbibresource"))
(add-hook 'reftex-toc-mode-hook
	      (lambda () (reftex-toc-rescan)))

;; set up mode for bib files
(with-eval-after-load 'bibtex
  (setq bibtex-dialect 'biblatex
        bibtex-align-at-equal-sign t
        bibtex-text-indentation 20))

(with-eval-after-load 'tex
  ;; company-auctex
  ;; dependencies: yasnippet company auctex
  (require 'company-auctex)
  (add-to-list '+latex-company-backends 'company-auctex-bibs)
  (add-to-list '+latex-company-backends 'company-auctex-labels)
  (add-to-list '+latex-company-backends 'company-auctex-symbols)
  (add-to-list '+latex-company-backends 'company-auctex-environments)
  (add-to-list '+latex-company-backends 'company-auctex-macros)

  ;; company-reftex
  ;; dependencies: s company
  (require 'company-reftex)
  (add-to-list '+latex-company-backends 'company-reftex-labels)
  (add-to-list '+latex-company-backends 'company-reftex-citations)

  ;; company-math
  ;; dependencies: company math-symbol-lists
  (require 'company-math)
  (add-to-list '+latex-company-backends 'company-math-symbols-latex)
  (add-to-list '+latex-company-backends 'company-latex-commands))

(defun zenith/latex-company-setup ()
  "Setup company backends for latex editing."
   (make-local-variable 'company-backends)
   (add-to-list 'company-backends +latex-company-backends))

(add-hook 'LaTeX-mode-hook 'zenith/latex-company-setup)


(autoload 'TeX-latex-mode "tex-site" "" t)
(add-hook 'latex-mode-hook 'TeX-latex-mode)

;; auctex
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
  (setq-default
   TeX-engine 'xetex
   TeX-show-compilation nil)
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
  ;; set default pdf viewer
  (add-to-list 'TeX-view-program-selection '(output-pdf "Zathura"))
  ;; set-up chktex
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")

  (add-to-list 'TeX-command-list
             '("XeLaTeX" "xelatex -interaction=nonstopmode %s"
               TeX-run-command t t :help "Run xelatex") t))

;; tell emacs how to parse tex files
(add-hook 'TeX-mode-hook (lambda () (setq ispell-parser 'tex)))
;; Fold TeX macros
(add-hook 'TeX-mode-hook #'TeX-fold-mode)
;; Enable rainbow mode after applying styles to the buffer
(add-hook 'TeX-mode-hook #'rainbow-delimiters-mode)
;; Do not prompt for Master files, this allows auto-insert to add templates to
;; .tex files
(add-hook 'TeX-mode-hook
	      ;; Necessary because it is added as an anonymous, byte-compiled function
	      (lambda ()
            (remove-hook 'find-file-hook
			             (cl-find-if #'byte-code-function-p find-file-hook)
			             'local)))

(defun LaTeX-star-environment-dwim ()
  "Convert between the starred and the not starred version of the current environment."
  (interactive)
  ;; If the current environment is starred.
  (if (string-match "\*$" (LaTeX-current-environment))
      ;; Remove the star from the current environment.
      (LaTeX-modify-environment (substring (LaTeX-current-environment) 0 -1))
    ;; Else add a star to the current environment.
    (LaTeX-modify-environment (concat (LaTeX-current-environment) "*"))))

;; Exchange position of { and [ in latex mode to reduce the injury of the finger
(general-define-key
 :keymaps 'LaTeX-mode-map
 "、" (lambda ()(interactive)(self-insert-command 1 ?\\))
 "C-*" 'LaTeX-star-environment-dwim
 "[" (lambda ()(interactive)(self-insert-command 1 ?{))
 "{" (lambda ()(interactive)(self-insert-command 1 ?\[))
 )

(setq LaTeX-section-hook ; Add the toc entry to the sectioning hooks.
      '(LaTeX-section-heading
        LaTeX-section-title
        LaTeX-section-section)
      LaTeX-fill-break-at-separators nil
      LaTeX-item-indent 0)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)

(provide 'init-latex)
;;; init-latex.el ends here
