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

(add-hook 'minibuffer-setup-hook #'max-gc-limit)
(add-hook 'minibuffer-exit-hook #'reset-gc-limit)

(with-eval-after-load 'nox
  (add-to-list 'nox-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
  (add-hook 'nox--managed-mode-hook 'zenith/set-default-company-backends))

(zenith/add-hook '(c-mode-hook c++-mode-hook js-mode-hook python-mode-hook) 'zenith/nox-load-and-ensure)

(provide 'init-lsp)
;;; init-lsp.el ends here
