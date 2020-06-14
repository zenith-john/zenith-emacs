;;; init-utils.el ---  configure some useful packages -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;; smartparens
(require 'smartparens-config)

(setq-default sp-autoskip-closing-pair t) ;; manually add closing pair work as I like

(zenith/add-hook '(comint-mode-hook prog-mode-hook LaTeX-mode-hook org-mode-hook) 'smartparens-mode)

;; format-all
(autoload 'format-all-buffer "format-all" nil t)

;; ibuffer-projectile
;; dependencies: projectile
(add-hook 'ibuffer-hook
          (lambda ()
            (require 'ibuffer-projectile)
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))
            (setq-default ibuffer-formats
                          '((mark modified read-only " "
                             (name 18 18 :left :elide)
                             " "
                             (size 9 -1 :right)
                             " "
                             (mode 16 16 :left :elide)
                             " "
                             project-relative-file)))))

;; ws-butler
(require 'ws-butler)
(ws-butler-global-mode 1)

;; emacs-wgrep
(autoload 'wgrep-change-to-wgrep-mode "wgrep")
(setq wgrep-auto-save-buffer t)

;; Mode load
;; markdown-mode
(dolist (elt '(("\\.md\\'" . markdown-mode)
               ("\\.markdown\\'" . markdown-mode)))
  (add-to-list 'auto-mode-alist elt))

(autoload 'markdown-mode "markdown-mode")

;; rg.el
;; dependencies: s transient wgrep
(zenith/autoload '(rg rg-project rg-dwim-project-dir) "rg")

;; ace-window
;; dependencies: avy
(autoload 'ace-window "ace-window")
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-scope 'frame
      aw-background t)

;; helpful
;; dependencies: dash dash-functional s f elisp-refs
;; elisp-refs depends on loop
(zenith/autoload '(helpful-callable helpful-variable helpful-key) "helpful")

;; avy
(zenith/autoload '(avy-goto-char avy-goto-char-in-line) "avy")
(setq avy-background t
      avy-all-windows nil)

;; ctrlf
(require 'ctrlf)
(ctrlf-mode 1)

(defun zenith/ctrlf-toggle-regex ()
  "Toggle regex search for ctrlf"
  (interactive)
  (ctrlf-change-search-style
   (pcase ctrlf--style
     ('fuzzy 'fuzzy-regexp)
     ('fuzzy-regexp 'fuzzy)
     ('literal 'regexp)
     ('regexp 'literal))))

(defun zenith/ctrlf-toggle-fuzzy ()
  "Toggle fuzzy for ctrlf."
  (interactive)
  (ctrlf-change-search-style
   (pcase ctrlf--style
     ('fuzzy 'literal)
     ('literal 'fuzzy)
     ('fuzzy-regexp 'regexp)
     ('regexp 'fuzzy-regexp))))

(defun zenith/ctrlf-next-match ()
  "Move to the next match of previous search"
  (interactive)
  (ctrlf--search (first ctrlf-search-history) :bound 'wraparound))

(defun zenith/ctrlf-previous-match ()
  "Move to the previous match of the previous search."
  (interactive)
  (ctrlf--search (first ctrlf-search-history) :backward t :bound 'wraparound))

(add-to-list 'ctrlf-minibuffer-bindings
             '("M-f" . zenith/ctrlf-toggle-fuzzy))
(add-to-list 'ctrlf-minibuffer-bindings
             '("M-d" . zenith/ctrlf-toggle-regex))

;; spell check by ispell
(setq-default ispell-program-name (executable-find "hunspell")
              ispell-dictionary "en_US"
              ispell-silently-savep t)

(require 'wucuo)

(setq wucuo-flyspell-start-mode "fast")

(defun zenith/flyspell-check-region ()
  "Remove overlay and check region"
  (interactive)
  (let ((wucuo-flyspell-start-mode "fast"))
    (wucuo-spell-check-buffer)))

(defun zenith/flyspell-check-buffer ()
  "Remove overlay and check buffer"
  (interactive)
  (let ((wucuo-flyspell-start-mode "normal"))
    (wucuo-spell-check-buffer)))

(defun zenith/add-word-to-dictionary (beg end)
  "Add word at point to the dictionary"
  (interactive "r")
  (save-excursion
    (let* ((word (concat (if (region-active-p)
                             (buffer-substring-no-properties beg end)
                           (word-at-point))
                         "\n"))
           (file (expand-file-name (concat "~/.hunspell_" ispell-dictionary))))
      (append-to-file word nil file)))
  (zenith/flyspell-check-region))

(add-hook 'after-save-hook 'zenith/flyspell-check-region)

;; Delete word in a more user friendly way
(defun zenith/is-space (char)
  "Check a char is whether a space character."
  (string-match (char-to-string char) "\t\n\r "))

(defun zenith/aggressive-delete-space ()
  "Remove all the space until non-space character."
  (interactive)
  (let ((end (point))
        (begin (save-excursion
                 (re-search-backward "[^ \t\n\r]" nil t))))
    (delete-region (+ 1 begin) end)))

(defun zenith/delete-word-or-space ()
  "Remove all the space until non-space character if the char at point and
before are all space characters and delete word otherwise."
  (interactive)
  (if (and (zenith/is-space (char-before))
           (zenith/is-space (char-before (- (point) 1))))
      (zenith/aggressive-delete-space)
    (backward-kill-word 1)))

(defun zenith/fill-and-indent-region ()
  "Fill paragraph and indent region at once"
  (interactive)
  (call-interactively 'fill-paragraph)
  (call-interactively 'indent-region))

(provide 'init-utils)
