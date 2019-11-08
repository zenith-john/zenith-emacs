;;; init-python.el --- python configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(setq lsp-python-ms-dir
      (concat zenith-emacs-extension-dir "python-language-server/output/bin/Release/"))
(add-hook 'python-mode-hook
          (lambda ()
            (require 'lsp-python-ms)
            (lsp)))

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

(use-package pyvenv
  :commands (pyvenv-activate
             pyvenv-workon))

(use-package py-isort
  :commands (py-isort-buffer))

(use-package pyimport
  :commands (pyimport-remove-unused pyimport-insert-missing))

(use-package yapfify
  :commands (yapfify-region yapfify-buffer))

(use-package conda
  :init (setq conda-anaconda-home (expand-file-name "~/anaconda3/"))
  :commands (conda-env-activate conda-env-deactivate))

(provide 'init-python)
;;; init-python.el ends here
