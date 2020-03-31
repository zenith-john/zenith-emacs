;;; init-lsp.el --- lsp-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; nox
(defun zenith/nox-load-and-ensure ()
  (require 'nox)
  (nox-ensure))

(zenith/add-hook '(c-mode-hook c++-mode-hook python-mode-hook) 'zenith/nox-load-and-ensure)

(provide 'init-lsp)
;;; init-lsp.el ends here
