;;; init-python.el --- python configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(use-package lsp-python-ms
  :hook (python-mode . lsp)
  :init
  (setq lsp-python-ms-dir
        (concat zenith-emacs-extension-dir "python-language-server/output/bin/Release/"))
  (require 'lsp-python-ms))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package python-pytest
  :commands
  (
   python-pytest
   python-pytest-file
   python-pytest-file-dwim
   python-pytest-function
   python-pytest-function-dwim
   python-pytest-last-failed
   python-pytest-repeat
   python-pytest-popup
   ))

(use-package py-isort
  :commands (py-isort-buffer))

(use-package pyimport
  :commands (pyimport-remove-unused pyimport-insert-missing))

(provide 'init-python)
;;; init-python.el ends here
