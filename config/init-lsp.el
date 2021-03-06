;;; init-lsp.el --- language server configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; nox
(defun zenith/nox-load-and-ensure ()
  (require 'nox)
  (nox-ensure))

(setq nox-optimization-p nil)

(defun zenith/max-read-process ()
  (setq read-process-output-max (* 20 1024 1024)))
(defun zenith/normal-read-process ()
  (setq read-process-output-max (* 1024 1024)))
(add-hook 'company-completion-started-hook (lambda (arg) (zenith/max-read-process)))
(add-hook 'company-completion-cancelled-hook (lambda (arg) (zenith/normal-read-process)))
(add-hook 'company-completion-finished-hook (lambda (arg) (zenith/normal-read-process)))

(with-eval-after-load 'nox
  (add-to-list 'nox-server-programs '(js-mode . ("typescript-language-server" "--stdio"))))

(when (executable-find "ccls")
  (zenith/add-hook '(c-mode-hook c++-mode-hook) 'zenith/nox-load-and-ensure))
(when (executable-find "typescript-language-server")
  (zenith/add-hook '(js-mode-hook) 'zenith/nox-load-and-ensure))
(when (executable-find "mspyls")
  (zenith/add-hook '(python-mode-hook) 'zenith/nox-load-and-ensure))

(provide 'init-lsp)
;;; init-lsp.el ends here
