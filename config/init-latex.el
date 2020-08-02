;;; init-latex.el --- AucTex configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(defvar +latex-company-backends '())

;; helm-bibtex
;; dependencies: swiper parsebib s dash f biblio
(autoload 'ivy-bibtex "ivy-bibtex" "" t)

(defun zenith/ivy-bibtex-insert-citation (candidate)
  (insert (format "\\cite{%s}" (cdr (assoc "=key=" (cdr candidate))))))

(setq bibtex-completion-bibliography zenith/bibtex-library
      bibtex-completion-additional-search-fields '("abstract")
      bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator ""
      bibtex-autokey-year-title-separator ""
      bibtex-autokey-titlewords 0
      ivy-bibtex-default-action 'zenith/ivy-bibtex-insert-citation)

;; ebib
;; depedencies: parsebib
(autoload 'ebib "ebib" nil t)
(setq ebib-preload-bib-files `(,zenith/bibtex-library))

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; Get ReTeX working with biblatex
;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#3199

(defun reftex-format-cref (label form style)
  (format "~\\cref{%s}" label))

(setq LaTeX-reftex-cite-format-auto-activate nil
      reftex-cite-format 'default
      reftex-toc-split-windows-horizontally t
      reftex-plug-into-AUCTeX t
      reftex-toc-split-windows-fraction 0.3
      reftex-bibpath-environment-variables '("/home/zenith-john/Dropbox/")
      reftex-bibliography-commands '("bibliography" "nobibiliography" "addbibresource")
      reftex-ref-macro-prompt nil
      reftex-format-ref-function 'reftex-format-cref)

(defun zenith/reftex-label-alist-toggle (&optional multi)
  (interactive)
  (if (and (not multi)
           reftex-label-alist
           (string-suffix-p "%f" (nth 3 (first reftex-label-alist))))
      (setq-local reftex-label-alist
                  '(("theorem" ?h "thm:" "~\\ref{%s}" nil (regexp "[Tt]heorem" "[Tt]h\\.") -3)
                    ("lemma"   ?l "lem:" "~\\ref{%s}" nil (regexp "[Ll]emma"   "[Ll]m\\.") -3)
                    ("proposition" ?p "prop:" "~\\ref{%s}" nil (regexp "[Pp]roposition" "[Pp]rop\\.") -3)
                    ("remark"      ?r "rmk:"  "~\\ref{%s}" nil (regexp "[Rr]emark" "[Rr]mk\\.") -3)
                    ("definition"  ?d "def:"  "~\\ref{%s}" nil (regexp "[Dd]efinition" "[Dd]ef\\.") -3)
                    ("corollary"   ?c "cor:"  "~\\ref{%s}" t (regexp "[Cc]orollary" "[Cc]or\\.") -3)
                    ("exercise"    ?g "ex:"   "~\\ref{%s}" nil (regexp "[Ex]ercise" "[Ee]x.") -3)
                    ("equation"  ?e "eq:" "~\\eqref{%s}" t (regexp "equations?" "eqs?\\." "eqn\\." "Gleichung\\(en\\)?"  "Gl\\."))
                    ("eqnarray"  ?e "eq:" nil eqnarray-like)))
    (setq-local reftex-label-alist
                '(("theorem" ?h "thm:%f-" "~\\ref{%s}" nil (regexp "[Tt]heorem" "[Tt]h\\.") -3)
                  ("lemma"   ?l "lem:%f-" "~\\ref{%s}" nil (regexp "[Ll]emma"   "[Ll]m\\.") -3)
                  ("proposition" ?p "prop:%f-" "~\\ref{%s}" nil (regexp "[Pp]roposition" "[Pp]rop\\.") -3)
                  ("remark"      ?r "rmk:%f-"  "~\\ref{%s}" nil (regexp "[Rr]emark" "[Rr]mk\\.") -3)
                  ("definition"  ?d "def:%f-"  "~\\ref{%s}" nil (regexp "[Dd]efinition" "[Dd]ef\\.") -3)
                  ("corollary"   ?c "cor:%f-"  "~\\ref{%s}" nil (regexp "[Cc]orollary" "[Cc]or\\.") -3)
                  ("exercise"    ?g "ex:"   "~\\ref{%s}" nil (regexp "[Ex]ercise" "[Ee]x.") -3)
                  ("equation"  ?e "eq:%f-" "~\\eqref{%s}" t (regexp "equations?" "eqs?\\." "eqn\\." "Gleichung\\(en\\)?"  "Gl\\."))
                  ("eqnarray"  ?e "eq:%f-" nil eqnarray-like)))))

(defvar zenith/label-kinds
  (rx (or "thm:" "lem:" "prop:" "rmk:" "def:" "cor:" "eq:" "ex:")))

(defun zenith/update-label-and-reference ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((file-name (zenith/get-bare-file-name)))
      (while (re-search-forward zenith/label-kinds nil t)
        (insert file-name "-")))))

;; counsel-reftex-labels
(defvar-local counsel-reftex-labels-candidates nil
  "Store the candidates for counsel-reftex-labels")

(defun counsel-reftex-labels--candidates ()
  "Find all label candidates"
  (reftex-parse-one)
  (cl-loop for entry in (symbol-value reftex-docstruct-symbol)
           if (stringp (car entry))
           collect
           (propertize (concat (car entry) "  " (cl-caddr entry)) 'label (car entry))))

(defun counsel-reftex-labels--get-labels (str)
  (or (get-text-property 0 'label str)
      str))

(defun counsel-reftex-labels-cref (str)
  (delete-horizontal-space t)
  (insert (format "~\\cref{%s}"
                  (counsel-reftex-labels--get-labels str))))

(defun counsel-reftex-labels-multi-action (candidates)
  "Multi action for counsel-reftex-labels"
  (let ((action (ivy--get-action ivy-last))
        (str ""))
    (dolist (cand candidates)
      (setq str
            (concat str (counsel-reftex-labels--get-labels cand) ",")))
    (funcall action (substring-no-properties str 0 (- (length str) 1)))))

(defun counsel-reftex-labels ()
  "Choose label"
  (interactive)
  (setq counsel-reftex-labels-candidates (counsel-reftex-labels--candidates))
  (ivy-read "Choose label: " counsel-reftex-labels-candidates
            :multi-action #'counsel-reftex-labels-multi-action
            :action #'counsel-reftex-labels-cref
            :caller 'counsel-reftex-labels))

(defun counsel-reftex-labels-pageref (str)
  (delete-horizontal-space t)
  (insert (format "~\\cpageref{%s}"
                  (counsel-reftex-labels--get-labels str))))

(defun counsel-reftex-labels-plain-string (str)
  (insert (counsel-reftex-labels--get-labels str)))

(ivy-add-actions
 'counsel-reftex-labels
 '(("p" counsel-reftex-labels-pageref "cpageref" counsel-reftex-labels-multi-action)
   ("s" counsel-reftex-labels-plain-string "plain string" counsel-reftex-labels-multi-action)))

;; set up mode for bib files
(with-eval-after-load 'bibtex
  (bibtex-set-dialect 'biblatex)
  (setq bibtex-align-at-equal-sign t
        bibtex-text-indentation 20))

(with-eval-after-load 'tex
  ;; the order of company-backend is important.
  ;; company-math
  ;; dependencies: company math-symbol-lists
  (require 'company-math)
  (require 'company-auctex)
  (require 'company-reftex)
  (add-to-list '+latex-company-backends 'company-auctex-environments)
  (add-to-list '+latex-company-backends 'company-auctex-macros)
  (add-to-list '+latex-company-backends 'company-math-symbols-latex)
  (add-to-list '+latex-company-backends 'company-reftex-labels)
  (add-to-list '+latex-company-backends 'company-reftex-citations))

(defun zenith/latex-company-setup ()
  "Setup company backends for latex editing."
  (setq-local company-combine-local-backends +latex-company-backends))

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
   TeX-show-compilation nil
   ;; fill paragraph should leave line equation in a line
   LaTeX-fill-break-at-separators '(\\\( \\\[ \\\] \})
   TeX-command-extra-options "-shell-escape")

  (setq LaTeX-section-hook
        '(LaTeX-section-heading
          LaTeX-section-title
          LaTeX-section-section
          LaTeX-section-label)
        LaTeX-item-indent 0)
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
          ;; hyperref
          ("autoref" "{")
          ("nameref" "{")
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
  ;; Set-up latexmk
  (require 'auctex-latexmk)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup)

  (defun zenith/latexmk-compile ()
    (interactive)
    (TeX-command "LatexMk" 'TeX-master-file))

  (defun zenith/latex-watch ()
    (interactive)
    (let ((display-buffer-alist '(("*Async Shell Command*" . (display-buffer-no-window)))))
      (async-shell-command (format "latexmk -pvc -view=none -cd -r ~/.latexmkrc %s"
                                   (expand-file-name (TeX-master-file t))))))

  (defun LaTeX-star-environment-dwim ()
    "Convert between the starred and the not starred version of the current environment."
    (interactive)
    ;; If the current environment is starred.
    (if (string-match "\*$" (LaTeX-current-environment))
        ;; Remove the star from the current environment.
        (LaTeX-modify-environment (substring (LaTeX-current-environment) 0 -1))
      ;; Else add a star to the current environment.
      (LaTeX-modify-environment (concat (LaTeX-current-environment) "*"))))

  (defun zenith/latex-toggle-section-with-star ()
    (interactive)
    (if (member '("section" 2) LaTeX-section-list) ;; TODO: Make it more robust.
        (setq LaTeX-section-list
              '(("part" 0)
                ("chapter" 1)
                ("section*" 2)
                ("subsection*" 3)
                ("subsubsection*" 4)
                ("paragraph" 5)
                ("subparagraph" 6)
                ("section" 2)
                ("subsection" 3)
                ("subsubsection" 4)))
      (setq LaTeX-section-list
            '(("part" 0)
              ("chapter" 1)
              ("section" 2)
              ("subsection" 3)
              ("subsubsection" 4)
              ("paragraph" 5)
              ("subparagraph" 6)))))

  (defvar zenith/equation-env-list
    '(("\\begin{equation}" . "\\end{equation}")
      ("\\(" . "\\)")
      ("\\[" . "\\]"))
    "The pairs of equation environment")

  (defun zenith/regex-or (l)
    (let ((regex "\\(?:")
          (first-one t))
      (dolist (e l)
        (if (not first-one)
            (setq regex
                  (concat regex "\\\|"))
          (setq first-one nil))
        (setq regex
              (concat regex (regexp-quote e))))
      (concat regex "\\)")))

  (defun zenith/equation-match (beg end)
    "Check whether `beg' and `end' matches as equation"
    (let ((beg-string (buffer-substring-no-properties beg (min (+ beg 20) (point-max))))
          (end-string (buffer-substring-no-properties (max (point-min) (- end 20)) end))
          ret)
      (dolist (e zenith/equation-env-list)
        (when (and
               (string-prefix-p (car e) beg-string)
               (string-suffix-p (cdr e) end-string))
          (setq ret e)))
      ret))

  (defun zenith/cycle-equation ()
    (interactive)
    (if-let* ((regex (zenith/regex-or (append (mapcar 'car zenith/equation-env-list)
                                              (mapcar 'cdr zenith/equation-env-list))))
              (beg (save-excursion (re-search-backward regex nil t)))
              (end (save-excursion (re-search-forward regex nil t)))
              (kind (zenith/equation-match beg end))
              (len (safe-length zenith/equation-env-list))
              (pos (cl-position kind zenith/equation-env-list))
              (next (nth (if (= pos (- len 1))
                             0
                           (+ pos 1)) zenith/equation-env-list)))
        (progn
          (save-excursion
            (goto-char beg)
            (delete-char (length (car kind)))
            (insert (car next))
            (re-search-forward (zenith/regex-or (mapcar
                                                 'cdr zenith/equation-env-list)))
            (delete-backward-char (length (cdr kind)))
            (insert (cdr next))))
      (message "No match equation environment found.")))

  ;; Some useful utilities to reduce the usage of shift.
  (defun zenith/latex-magic-bracket ()
    (interactive)
    (let ((char (char-before)))
      (if (eq char ?\{)
          (progn
            (backward-delete-char 1)
            (funcall-interactively 'self-insert-command 1 ?\[))
        (if (or (zenith/is-char char)
                (and (eq char ?\\) (texmathp)))
            (funcall-interactively 'self-insert-command 1 ?\{)
          (funcall-interactively 'self-insert-command 1 ?\[)))))

  (defun zenith/insert-underscore ()
    (interactive)
    (funcall-interactively 'self-insert-command 1 ?_)
    (when (and TeX-electric-sub-and-superscript (texmathp))
      (insert (concat TeX-grop TeX-grcl))
      (backward-char)))

  (defun zenith/latex-magic-underscore ()
    (interactive)
    (if (and (texmathp)
             (not (zenith/is-space (char-before)))
             (not (zenith/is-bra (char-before))))
        (progn
          (funcall-interactively 'self-insert-command 1 ?_)
          (when (and TeX-electric-sub-and-superscript (texmathp))
            (insert (concat TeX-grop TeX-grcl))
            (backward-char)))
      (funcall-interactively 'self-insert-command 1 ?-)))

  (defun zenith/latex-magic-k ()
    (interactive)
    (if (equal (char-before) ?k)
        (progn
          (backward-delete-char 1)
          (insert "\\k"))
      (if (equal (char-before) ?\\)
          (progn
            (backward-delete-char 1)
            (self-insert-command 1 ?k))
        (if (zenith/is-space (char-before))
            (self-insert-command 1 ?\\)
          (self-insert-command 1 ?k)))))

  (defun zenith/latex-insert-quote ()
    "Baby version insert quote for latex-mode"
    (interactive)
    (if (equal (char-before) ?\\)
        (insert "\"")
      (if (texmathp)
          (progn
            (insert "\"\"")
            (backward-char))
        (insert "``''")
        (backward-char 2)))))

;; Set up LaTeX-mode hooks
(defun zenith/update-after-save-hook ()
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook '(lambda ()
                                (TeX-update-style t))))

(defun zenith/latex-mode-hook ()
  ;; Set up after-save-hook
  (zenith/update-after-save-hook)
  ;; Set up environment for LaTeX
  (LaTeX-add-environments
   '("tikzcd" LaTeX-env-label))
  (zenith/reftex-label-alist-toggle t))

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

(add-hook 'LaTeX-mode-hook 'zenith/latex-mode-hook)

(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

(provide 'init-latex)
;;; init-latex.el ends here
