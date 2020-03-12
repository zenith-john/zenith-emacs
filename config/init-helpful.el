;;; init-helpful.el --- helpful configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; helpful
;; dependencies: dash dash-functional s f elisp-refs
;; elisp-refs depends on loop
(zenith/autoload '(helpful-callable helpful-variable helpful-key) "helpful")

(general-def
  "C-h f" #'helpful-callable
  "C-h v" #'helpful-variable
  "C-h k" #'helpful-key)

(provide 'init-helpful)
;;; init-helpful.el ends here
