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

(defun zenith/autoload (list file)
  "Autoload multiple function of the file at once"
  (dolist (elt list)
    (autoload elt file)))

(provide 'init-func)
;;; init-func.el ends here
