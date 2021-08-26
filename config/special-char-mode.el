;;; Special Char Mode
;; A minor mode that convert number key to punctures.

;; Special Char Mode
(defun zenith/special-char-environment-p ()
  ;; In LaTeX math environment, or in code environment.
  (cond ((eq major-mode 'latex-mode) t)
        ((eq major-mode 'org-mode) t)
        ((derived-mode-p 'prog-mode)
         (if (or (nth 4 (syntax-ppss))
                 (nth 8 (syntax-ppss)))
             nil
           t))
        (t nil)))

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
  :lighter " SpecialChar" :global nil
  :keymap special-char-mode-map)

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

(defun zenith/latex-super-script ()
  (interactive)
  (funcall-interactively 'self-insert-command 1 ?^)
  (when (and TeX-electric-sub-and-superscript
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

(provide 'special-char-mode)
