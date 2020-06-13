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
                         (narrow-to-region (line-beginning-position) (line-end-position))
                         (search-backward-regexp "^\\|[[:blank:]]\\|(\\|)\\|\\[\\|]\\|{\\|}" nil t))))
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

(defun zenith/post-self-insert-hook ()
  "Check whether or not to expand after insertion of ~SPC~."
  (interactive)
  (when (eq (char-before) ?\s)
    (delete-backward-char 1)
    (unless (zenith/may-expand)
      (insert-char ?\s)
      (when (boundp company-mode)
        (company-abort)))))

(add-hook 'post-self-insert-hook 'zenith/post-self-insert-hook t)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
