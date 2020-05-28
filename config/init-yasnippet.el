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

(defun zenith/remove-comma-for-word-at-point ()
  (interactive)
  (save-excursion
    (let ((word-end (point))
          (word-start (search-backward-regexp "[[:blank:]\n\r]")))
      (when (string-prefix-p ","
                             (buffer-substring-no-properties
                              (+ word-start 1) word-end))
          (delete-region (+ word-start 1) (+ word-start 2))))))

(defun zenith/may-expand ()
  (interactive)
  (let* ((word-end (point))
         (word-start (save-excursion (search-backward-regexp "[[:blank:]\n\r]" nil t)))
         (word))

    (when word-start
      (setq word (buffer-substring-no-properties (+ word-start 1) word-end))
      (when (string-prefix-p "," word)
        (delete-region (+ word-start 1) (+ word-start 2))
        (call-interactively 'yas-expand)))))

(defun zenith/post-self-insert-hook ()
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
