;;; init-func.el --- define useful functions -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(defun +clear-image-cache ()
  "Remove image cache to redisplay the image."
  (interactive)
  (clear-image-cache))

(defun +enable-paredit-mode ()
  (require 'paredit)
  (paredit-mode 1))

(provide 'init-func)
;;; init-func.el ends here
