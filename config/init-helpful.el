;;; init-helpful.el --- helpful configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; helpful
;; dependencies: dash dash-functional s f elisp-refs
;; elisp-refs depends on loop
(zenith/autoload '(helpful-callable helpful-variable helpful-key) "helpful")

(provide 'init-helpful)
;;; init-helpful.el ends here
