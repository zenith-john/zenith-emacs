;;; init-lsp.el --- lsp-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; nox
(defun zenith/nox-load-and-ensure ()
  (require 'nox)
  (nox-ensure))

(setq read-process-output-max (* 1024 1024)
      nox-optimization-p nil)

(with-eval-after-load 'nox
  (add-to-list 'nox-server-programs '(js-mode . ("typescript-language-server" "--stdio"))))

(zenith/add-hook '(c-mode-hook c++-mode-hook js-mode-hook python-mode-hook) 'zenith/nox-load-and-ensure)

(provide 'init-lsp)
;;; init-lsp.el ends here
