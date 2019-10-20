;;; init-func.el --- define useful functions -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(defun +clear-image-cache ()
  "Remove image cache to redisplay the image."
  (interactive)
  (clear-image-cache))

(provide 'init-func)
;;; init-func.el ends here
