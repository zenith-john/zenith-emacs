;;; init-func.el --- define useful functions -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; fd-dired
(autoload 'fd-dired "fd-dired")

(autoload 'er/expand-region "expand-region")
(general-define-key
 "C-=" 'er/expand-region)
(general-define-key
 :keymaps 'normal
 "=" 'er/expand-region)

(defun +clear-image-cache ()
  "Remove image cache to redisplay the image."
  (interactive)
  (clear-image-cache))

(defun zenith/autoload (command-list file)
  "Autoload multiple function of the file at once"
  (dolist (command command-list)
    (autoload command file nil t)))

(defun zenith/add-hook (hook-list func)
  (dolist (hook hook-list)
    (add-hook hook func)))

(provide 'init-func)
;;; init-func.el ends here
