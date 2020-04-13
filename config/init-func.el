;;; init-func.el --- define useful functions -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; fd-dired
(autoload 'fd-dired "fd-dired")

(autoload 'er/expand-region "expand-region")

(defun +clear-image-cache ()
  "Remove image cache to redisplay the image."
  (interactive)
  (clear-image-cache))

(defun zenith/autoload (command-list file &optional non-interactive)
  "Autoload multiple function of the file at once"
  (dolist (command command-list)
    (autoload command file nil (not non-interactive))))

(defun zenith/add-hook (hook-list func)
  (dolist (hook hook-list)
    (add-hook hook func)))

(defun zenith/delay-load (func)
  (run-with-idle-timer 1 nil
                       func))

(provide 'init-func)
;;; init-func.el ends here
