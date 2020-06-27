;;; init-func.el --- define useful functions -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; fd-dired
(autoload 'fd-dired "fd-dired" nil t)

(autoload 'er/expand-region "expand-region" nil t)

(defun +clear-image-cache ()
  "Remove image cache to redisplay the image."
  (interactive)
  (clear-image-cache))

(defun zenith/autoload (command-list file &optional non-interactive)
  "Autoload multiple function of the file at once"
  (dolist (command command-list)
    (autoload command file nil (not non-interactive))))

(defun zenith/add-hook (hook-list func)
  "Add one function to multiple hooks at once"
  (dolist (hook hook-list)
    (add-hook hook func)))

(defun zenith/delay-load (func)
  "Delay the evaluation of the function"
  (run-with-idle-timer 1 nil
                       func))

(defun zenith/is-space (char)
  "Check a char is whether a space character."
  (string-match (regexp-quote (char-to-string char)) "\t\n\r "))

(defun zenith/get-bare-file-name (&optional buffer)
  (let ((file (buffer-file-name buffer)))
    (file-name-sans-extension (file-name-base file))))


(provide 'init-func)
;;; init-func.el ends here
