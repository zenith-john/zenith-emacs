;;; init-utils.el ---  configure some useful packages -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; smartparens
(require 'smartparens-config)

(setq-default sp-autoskip-closing-pair t) ;; manually add closing pair work as I like

(with-eval-after-load 'smartparens-latex
  (sp-with-modes '(
                   tex-mode
                   plain-tex-mode
                   latex-mode
                   LaTeX-mode
                   )
    (sp-local-pair "``" "''" :actions :rem)
    ;; Temporarily disable that pair for more details see
    ;; https://github.com/Fuco1/smartparens/issues/772

    (sp-local-pair "\"" "\""
                   :unless '(sp-latex-point-after-backslash)))
  (sp-with-modes '(org-mode)
    (sp-local-pair "\\[" "\\]")))

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
(if (executable-find "rg")
    (progn
      (require 'rg)
      (rg-define-search rg-my-project
        :dir project
        :files "everything")

      (evil-define-command evil-rg (&optional search)
        "Invoke `my-rg-project' with SEARCH"
        (interactive "<a>")
        (if search
            (rg-my-project search)
          (rg-dwim-project-dir)))
      (evil-ex-define-cmd "rg" #'evil-rg))
  (defun rg-my-project ()
    (interactive)
    (message "Program rg is not found.")))

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

; spell check by ispell
(setq-default ispell-program-name (executable-find "hunspell")
              ispell-dictionary "en_US"
              ispell-silently-savep t)

(when ispell-program-name
  (setq wucuo-font-faces-to-check
        '(font-lock-string-face
          font-lock-doc-face
          font-lock-comment-face))

  (require 'wucuo)

  (setq
   wucuo-enable-camel-case-algorithm-p nil
   wucuo-flyspell-start-mode "fast")

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

  ;; Redefine `flyspell-external-point-words'
  ;;*---------------------------------------------------------------------*/
  ;;*    flyspell-external-point-words ...                                */
  ;;*---------------------------------------------------------------------*/
  (defun flyspell-external-point-words ()
    "Mark words from a buffer listing incorrect words in order of appearance.
The list of incorrect words should be in `flyspell-external-ispell-buffer'.
\(We finish by killing that buffer and setting the variable to nil.)
The buffer to mark them in is `flyspell-large-region-buffer'."
    (let* (words-not-found
           (flyspell-casechars (flyspell-get-casechars))
           (ispell-otherchars (ispell-get-otherchars))
           (ispell-many-otherchars-p (ispell-get-many-otherchars-p))
           (word-chars (concat flyspell-casechars
                               "+\\("
                               (if (not (string= "" ispell-otherchars))
                                   (concat ispell-otherchars "?"))
                               flyspell-casechars
                               "+\\)"
                               (if ispell-many-otherchars-p
                                   "*" "?")))
           (buffer-scan-pos flyspell-large-region-beg)
           case-fold-search)
      (with-current-buffer flyspell-external-ispell-buffer
        (goto-char (point-min))
        ;; Loop over incorrect words, in the order they were reported,
        ;; which is also the order they appear in the buffer being checked.
        (while (re-search-forward "\\([^\n]+\\)\n" nil t)
          ;; Bind WORD to the next one.
          (let ((word (match-string 1)) (wordpos (point)))
            ;; Here there used to be code to see if WORD is the same
            ;; as the previous iteration, and count the number of consecutive
            ;; identical words, and the loop below would search for that many.
            ;; That code seemed to be incorrect, and on principle, should
            ;; be unnecessary too. -- rms.
            (if flyspell-issue-message-flag
                (message "Spell Checking...%d%% [%s]"
                         (floor (* 100.0 (point)) (point-max))
                         word))
            (with-current-buffer flyspell-large-region-buffer
              (goto-char buffer-scan-pos)
              (let ((keep t))
                ;; Iterate on string search until string is found as word,
                ;; not as substring.
                (while keep
                  (if (search-forward word
                                      flyspell-large-region-end t)
                      (let* ((found-list
                              (save-excursion
                                ;; Move back into the match
                                ;; so flyspell-get-word will find it.
                                (forward-char -1)
                                ;; Is this a word that matches the
                                ;; current dictionary?
                                (if (looking-at word-chars)
                                    (flyspell-get-word))))
                             (found (car found-list))
                             (found-length (length found))
                             (misspell-length (length word)))
                        (when (or
                               ;; Misspelled word is not from the
                               ;; language supported by the current
                               ;; dictionary.
                               (null found)
                               ;; Size matches, we really found it.
                               (= found-length misspell-length)
                               ;; Matches as part of a boundary-char separated
                               ;; word.
                               (member word
                                       (split-string found ispell-otherchars))
                               ;; Misspelling has higher length than
                               ;; what flyspell considers the word.
                               ;; Caused by boundary-chars mismatch.
                               ;; Validating seems safe.
                               (< found-length misspell-length)
                               ;; ispell treats beginning of some TeX
                               ;; commands as nroff control sequences
                               ;; and strips them in the list of
                               ;; misspelled words thus giving a
                               ;; non-existent word.  Skip if ispell
                               ;; is used, string is a TeX command
                               ;; (char before beginning of word is
                               ;; backslash) and none of the previous
                               ;; conditions match.
                               (and (not ispell-really-aspell)
                                    (not ispell-really-hunspell)
                                    (not ispell-really-enchant)
                                    (save-excursion
                                      (goto-char (- (nth 1 found-list) 1))
                                      (if (looking-at "[\\]" )
                                          t
                                        nil))))
                          (setq keep nil)
                          ;; Don't try spell-checking words whose
                          ;; characters don't match CASECHARS, because
                          ;; flyspell-word will then consider as
                          ;; misspelling the preceding word that matches
                          ;; CASECHARS.
                          (or (null found)
                              (flyspell-word nil t))
                          ;; Search for next misspelled word will begin from
                          ;; end of last validated match.
                          (setq buffer-scan-pos (point))))
                    ;; Record if misspelling is not found and try new one
                    (cl-pushnew (concat " -> " word " - "
                                        (int-to-string wordpos))
                                words-not-found :test #'equal)
                    (setq keep nil)))))))
        ;; we are done
        (if flyspell-issue-message-flag (message "Spell Checking completed.")))
      ;; Kill and forget the buffer with the list of incorrect words.
      (kill-buffer flyspell-external-ispell-buffer)
      (setq flyspell-external-ispell-buffer nil)))

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

  ;; Move point to previous error
  ;; based on code by hatschipuh at
  ;; http://emacs.stackexchange.com/a/14912/2017
  (defun flyspell-goto-previous-error (arg)
    "Go to arg previous spelling error."
    (interactive "p")
    (backward-word 1)
    (while (not (= 0 arg))
      (let ((pos (point))
            (min (point-min)))
        (if (and (eq (current-buffer) flyspell-old-buffer-error)
                 (eq pos flyspell-old-pos-error))
            (progn
              (if (= flyspell-old-pos-error min)
                  ;; goto beginning of buffer
                  (progn
                    (message "Restarting from end of buffer")
                    (goto-char (point-max)))
                (backward-word 1))
              (setq pos (point))))
        ;; seek the next error
        (while (and (> pos min)
                    (let ((ovs (overlays-at pos))
                          (r '()))
                      (while (and (not r) (consp ovs))
                        (if (flyspell-overlay-p (car ovs))
                            (setq r t)
                          (setq ovs (cdr ovs))))
                      (not r)))
          (backward-word 1)
          (setq pos (point)))
        ;; save the current location for next invocation
        (setq arg (1- arg))
        (setq flyspell-old-pos-error pos)
        (setq flyspell-old-buffer-error (current-buffer))
        (goto-char pos)
        (when (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0)))))
    (forward-word)))

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
  (unless visual-line-mode
    (call-interactively 'fill-paragraph)))

;; function to unfill
(defun unfill-paragraph ()
  "Do the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))

;; visual fill column
(autoload 'visual-fill-column-mode "visual-fill-column" "" t)
(setq-default visual-fill-column-width (+ fill-column 20)
              word-wrap-by-category t)
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

;; Special Char Mode
(defun zenith/special-char-environment-p ()
  ;; In LaTeX math environment, or in code environment.
  (if (eq major-mode 'latex-mode)
      t
    (if (derived-mode-p 'prog-mode)
        (if (or (nth 4 (syntax-ppss))
                  (nth 8 (syntax-ppss)))
            nil
          t)
      nil)))

(defun --is-number-or-dot (char)
  (or (and (>= char ?0) (<= char ?9))
      (eq char ?.)))

(defmacro ins-val (origin-var special-var)
  `(lambda () (interactive)
     (funcall-interactively 'self-insert-command 1
                            (if (and (char-before )(--is-number-or-dot (char-before)))
                                ,(if (--is-number-or-dot origin-var)
                                     origin-var
                                   special-var)
                              (if (zenith/special-char-environment-p)
                                  ,special-var
                                ,origin-var)))))

(defvar special-char-mode-map
  (make-sparse-keymap)
  "Keymap for special-char-mode")

(define-minor-mode special-char-mode
  "Toggle Special Character mode"
  nil
  " SpecialChar"
  special-char-mode-map
  :global nil)

(general-define-key
 :keymaps 'special-char-mode-map
 "M-n" 'zenith/insert-number
 "4"   (ins-val ?4 ?$)
 "$"   (ins-val ?$ ?4)
 "5"   (ins-val ?5 ?%)
 "%"   (ins-val ?% ?5)
 "6"   'zenith/smart-super-script
 "^"   (ins-val ?^ ?6)
 "7"   (ins-val ?7 ?&)
 "&"   (ins-val ?& ?7)
 "8"   (ins-val ?8 ?*)
 "*"   (ins-val ?* ?8)
 "9"   (ins-val ?9 ?\()
 "("   (ins-val ?\( ?9)
 "{"   (lambda ()(interactive)(self-insert-command 1 ?\]))
 "]"   (lambda ()(interactive)(self-insert-command 1 ?\{)))

(add-hook 'prog-mode-hook 'special-char-mode)
(add-hook 'LaTeX-mode-hook 'special-char-mode)

(defun zenith/latex-super-script ()
  (interactive)
  (funcall-interactively 'self-insert-command 1 ?^)
  (when (and (eq major-mode 'latex-mode)
             TeX-electric-sub-and-superscript
             (texmathp))
    (insert (concat TeX-grop TeX-grcl))
    (backward-char)))

(defun zenith/smart-super-script ()
  (interactive)
  (if (and
       (zenith/special-char-environment-p)
       (not (--is-number-or-dot (char-before))))
      (zenith/latex-super-script)
    (self-insert-command 1 ?6)))

(defun zenith/latex-sub-script ()
  (interactive)
  (funcall-interactively 'self-insert-command 1 ?_)
  (when (and (eq major-mode 'latex-mode)
             TeX-electric-sub-and-superscript
             (texmathp))
    (insert (concat TeX-grop TeX-grcl))
    (backward-char)))

(defun zenith/insert-number ()
  "Although named insert number, in fact it can insert everything."
  (interactive)
  (let ((special-char-mode-map nil))
    (insert (read-from-minibuffer "Insert Number: "))))

;; Dired enhancement
(require 'dired-filter)
(require 'dired-open)
(require 'dired-subtree)

;; emacs-libvterm
(if zenith/enable-vterm
    (progn
      (require 'vterm)
      (require 'multi-vterm)

      (defun zenith/vterm-toggle ()
        "Toggle vterm."
        (interactive)
        (if (projectile-project-p)
            (multi-vterm-project)
          (multi-vterm-dedicated-toggle))))
  (defun zenith/vterm-toggle ()
    (interactive)
    (message "Vterm module is not enabled.")))

;; emacs-winum
;; dependencies: cl-lib dash
(require 'winum)
(winum-mode)

;; auto-capitalize-el
;; (require 'auto-capitalize)
;; (setq auto-capitalize-words nil)
;; (add-hook 'after-change-major-mode-hook 'auto-capitalize-mode)

;; tramp
(require 'tramp)
(setq tramp-default-method "sshx")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; ledger-mode
(with-eval-after-load 'org
  (require 'ledger-mode))

(provide 'init-utils)
