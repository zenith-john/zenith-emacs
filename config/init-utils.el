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
(zenith/autoload '(ace-window ace-delete-window) "ace-window")
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

(setq wucuo-font-faces-to-check
      '(font-lock-string-face
        font-lock-doc-face
        font-lock-comment-face))

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

(add-hook 'text-mode-hook 'wucuo-start)
(add-hook 'prog-mode-hook 'wucuo-start)

;; Not enable it because checking makes cursor fly.

(defun zenith/add-word-to-dictionary (beg end)
  "Add word at point to the dictionary"
  (interactive "r")
  (save-excursion
    (let* ((word (concat (if (region-active-p)
                             (buffer-substring-no-properties beg end)
                           (word-at-point t))
                         "\n"))
           (file (expand-file-name (concat "~/.hunspell_" ispell-dictionary))))
      (append-to-file word nil file)))
  (zenith/flyspell-check-region))

;; Delete word in a more user friendly way
(defun zenith/aggressive-delete-space ()
  "Remove all the space until non-space character."
  (interactive)
  (let ((end (point))
        (begin (save-excursion
                 (re-search-backward "[^ \t\n\r]" nil t))))
    (delete-region (+ 1 begin) end)))

(defun zenith/delete-word-or-space ()
  "Remove all the space until non-space character if the char at
point and before are all space characters and delete word
otherwise."
  (interactive)
  (if (and (zenith/is-space (char-before))
           (zenith/is-space (char-before (- (point) 1))))
      (zenith/aggressive-delete-space)
    (backward-kill-word 1)))

(defun zenith/fill-and-indent-region ()
  "Fill paragraph and indent region at once"
  (interactive)
  (message "Never fill again, use format all"))
  ;; (when (or
  ;;        (derived-mode-p 'text-mode)
  ;;        (nth 4 (syntax-ppss))
  ;;        (nth 8 (syntax-ppss)))
  ;;   (call-interactively 'fill-paragraph))
  ;; (call-interactively 'indent-region))

;; visual fill column
(autoload 'visual-fill-column-mode "visual-fill-column" "" t)
(setq-default visual-fill-column-width fill-column)
(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
(add-hook 'auto-fill-mode-hook 'visual-line-mode)

;; jump in my way
(defvar zenith/jump-function-alist
  '((org-mode . org-goto)
    (latex-mode . reftex-toc)
    (org-agenda-mode . org-agenda-redo)
    (t . counsel-imenu))
  "The function to call when jump")

(defun zenith/jump ()
  "Jump as `zenith/jump-function-alist' like."
  (interactive)
  (if-let ((func (alist-get major-mode zenith/jump-function-alist)))
      (funcall func)
    (funcall (alist-get t zenith/jump-function-alist))))

;; Popup window management
;; shackle
(require 'shackle)
(setq shackle-default-alignment 'below
      shackle-default-size 0.33)

(defun zenith/get-most-below-window ()
  "Get the most below window"
  (interactive)
  (save-window-excursion
    (save-excursion
      (ignore-errors
        (dotimes (i 20)
          (windmove-down)))
      (selected-window))))

(defvar zenith/matching-popup-list
  '(" \\*"
    "\\*Help")
  "The regex suggest that the buffer is popup window")

(setq shackle-rules
      '((" \\*" :regexp t :custom zenith/shackle-custom-window)
        ("\\*Help" :regexp t :custom zenith/shackle-custom-window)))

(defun zenith/matching-popup (str)
  (let ((ret))
    (dolist (regexp zenith/matching-popup-list ret)
      (setq ret (or ret (string-match-p regexp str))))))

(defun zenith/shackle-custom-window (buffer alist plist)
  (let ((win (zenith/get-most-below-window)))
    (if (zenith/matching-popup (buffer-name (window-buffer win)))
        (shackle--window-display-buffer buffer win 'window alist)
      (shackle--display-buffer-aligned-window buffer alist plist))))

(shackle-mode)

(defmacro ins-val (val)
  `(lambda () (interactive)
     (funcall-interactively 'self-insert-command 1 ,val)))

(define-minor-mode special-char-mode
  "Toggle Special Character mode"
  nil
  " SpecialChar"
  `(
    (,(kbd "6") . zenith/latex-super-script)
    (,(kbd "^") . ,(ins-val ?6))
    (,(kbd "7") . ,(ins-val ?&)) (,(kbd "&") . ,(ins-val ?7))
    (,(kbd "8") . ,(ins-val ?*)) (,(kbd "*") . ,(ins-val ?8))
    (,(kbd "9") . ,(ins-val ?\()) (,(kbd "(") . ,(ins-val ?9))
    (,[kp-multiply] . ,(ins-val ?*))
    (,(kbd "]") . ,(ins-val ?\{))
    (,(kbd "{") . ,(ins-val ?\])))
  :global 'true)

(defun zenith/latex-super-script ()
  (interactive)
  (funcall-interactively 'self-insert-command 1 ?^)
  (when (and (eq major-mode 'latex-mode)
             TeX-electric-sub-and-superscript
             (texmathp))
    (insert (concat TeX-grop TeX-grcl))
    (backward-char)))

(provide 'init-utils)
