;;; init-yasnippet.el --- yasnippet configuration -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;; yasnippet

;; disable useless keybindings
(setq yas-minor-mode-map (make-sparse-keymap))
(require 'yasnippet)
(setq yas-snippet-dirs `(,(concat zenith-emacs-extension-dir "snippets/")))

(yas-global-mode +1)

;; Make yasnippet expansion easy for me.
(defvar zenith/snippet-prefix ?,
  "The first character of expanding yasnippet")

(defun zenith/may-expand ()
  "Auto expand if the word before the point are started with
`zenith/snippet-prefix'. Return `t' if the expansion is successful
and `nil' otherwise."
  (interactive)
  (let* ((word-end (point))
         (word-start (save-excursion
                       (save-restriction
                         (narrow-to-region (line-beginning-position 0) (line-end-position))
                         (search-backward-regexp "^\\|[[:blank:]]\\|(\\|)\\|\\[\\|]\\|{\\|}\\|\\$" nil t))))
         (word)
         (len))

    (when word-start
      (if (eq (char-after word-start) zenith/snippet-prefix)
          (setq word (buffer-substring-no-properties word-start word-end))
        (setq
         word-start (+ word-start 1)
         word (buffer-substring-no-properties word-start word-end)))
      (when (eq zenith/snippet-prefix (string-to-char word))
        (delete-region word-start (+ word-start 1))
        (if (call-interactively 'yas-expand)
            t
          (setq len (- (length word) 1))
          (backward-char len)
          (insert-char zenith/snippet-prefix)
          (forward-char len)
          nil)))))

(defun zenith/post-command-hook ()
  "Check whether or not to expand after insertion of ~SPC~."
  (interactive)
  (when
      (and
       yas-minor-mode
       (or (eq this-command 'self-insert-command)
           (eq this-command 'org-self-insert-command))
       (eq (char-before) ?\s))
    (delete-backward-char 1)
    (unless (zenith/may-expand)
      (insert-char ?\s)
      (when (boundp company-mode)
        (company-abort)))))

(define-minor-mode auto-expand-mode
  "Minor mode for zenith/may-expand"
  nil nil nil
  (if auto-expand-mode
      ;; Priority of the function should be high enough to run before fill
      ;; column
      (add-hook 'post-command-hook 'zenith/post-command-hook 0 t)
    (remove-hook 'post-command-hook 'zenith/post-command-hook t)))
(define-globalized-minor-mode global-auto-expand-mode auto-expand-mode auto-expand-mode-on)

(defun auto-expand-mode-on ()
  (auto-expand-mode 1))

(global-auto-expand-mode 1)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
