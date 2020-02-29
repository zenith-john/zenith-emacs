;;; init-func.el --- define useful functions -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; fd-dired
(use-package fd-dired
  :commands (fd-dired)
  :if (executable-find "fd"))

(use-package expand-region
  :commands (er/expand-region)
  :after evil
  :init
  (general-define-key
   "C-=" 'er/expand-region)
  (general-define-key
   :keymaps 'normal
   "=" 'er/expand-region))

(defun +clear-image-cache ()
  "Remove image cache to redisplay the image."
  (interactive)
  (clear-image-cache))

(provide 'init-func)
;;; init-func.el ends here
