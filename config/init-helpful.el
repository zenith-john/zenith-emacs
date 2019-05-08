;;; init-helpful.el --- helpful configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-key)
  :init
  (general-def
    "C-h f" #'helpful-callable
    "C-h v" #'helpful-variable
    "C-h k" #'helpful-key))

(provide 'init-helpful)
;;; init-helpful.el ends here
