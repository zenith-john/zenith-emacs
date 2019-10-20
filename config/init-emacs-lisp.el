;;; init-emacs-lisp.el --- emacs lisp configuration -*- lexical-binding: t; -*-

;; `auto-compile'
(setq auto-compile-display-buffer nil
      auto-compile-use-mode-line nil)

(use-package macrostep
  :commands (macrostep-expand))

(defun zenith/load-sly-el-indent ()
  (require 'sly-el-indent)
  (sly-el-indent-setup))
(add-hook 'emacs-lisp-mode-hook #'zenith/load-sly-el-indent)

;; From doom-emacs
(defvar +emacs-lisp--face nil)
;;;###autoload
(defun +emacs-lisp-highlight-vars-and-faces (end)
  "Match defined variables and functions.

Functions are differentiated into special forms, built-in functions and
library/userland functions"
  (catch 'matcher
    (while (re-search-forward "\\_<.+?\\_>" end t)
      (unless (save-excursion
                (let ((ppss (syntax-ppss)))
                  (or (nth 3 ppss) (nth 4 ppss))))
        (let ((symbol (intern-soft (match-string-no-properties 0))))
          (and (cond ((null symbol) nil)
                     ((eq symbol t) nil)
                     ((special-variable-p symbol)
                      (setq +emacs-lisp--face 'font-lock-variable-name-face))
                     ((and (fboundp symbol)
                           (eq (char-before (match-beginning 0)) ?\())
                      (let ((unaliased (indirect-function symbol)))
                        (unless (or (macrop unaliased)
                                    (special-form-p unaliased))
                          (let (unadvised)
                            (while (not (eq (setq unadvised (ad-get-orig-definition unaliased))
                                            (setq unaliased (indirect-function unadvised)))))
                            unaliased)
                          (setq +emacs-lisp--face
                                (if (subrp unaliased)
                                    'font-lock-constant-face
                                  'font-lock-function-name-face))))))
               (throw 'matcher t)))))
    nil))

;; `+emacs-lisp-highlight-vars-and-faces' is a potentially expensive function
;; and should be byte-compiled, no matter what, to ensure it runs as fast as
;; possible:
(when (not (byte-code-function-p (symbol-function '+emacs-lisp-highlight-vars-and-faces)))
  (with-no-warnings
    (byte-compile #'+emacs-lisp-highlight-vars-and-faces)))

;; Special fontification for elisp
(font-lock-add-keywords
 'emacs-lisp-mode
 (append `(;; custom Doom cookies
           ("^;;;###\\(autodef\\|if\\|package\\)[ \n]" (1 font-lock-warning-face t)))
         ;; highlight defined, special variables & functions
         `((+emacs-lisp-highlight-vars-and-faces . +emacs-lisp--face))))


(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode)
  (lisp-mode . rainbow-delimiters-mode))

(provide 'init-emacs-lisp)
;;; init-emacs-lisp.el ends here
