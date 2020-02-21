;;; init-python.el --- python configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; lsp-python-ms
;; dependencies: lsp-mode
(setq lsp-python-ms-dir
      (concat zenith-emacs-extension-dir "python-language-server/output/bin/Release/"))
(add-hook 'python-mode-hook
          (lambda ()
            (require 'lsp-python-ms)
            (lsp)))

;; pipenv
;; dependencies: pyvenv s f
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

;; emacs-python-pytest
;; dependencies: dash dash-functional magit-popup projectile s
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

;; pyvenv
(use-package pyvenv
  :commands (pyvenv-activate
             pyvenv-workon))

;; py-isort
(use-package py-isort
  :commands (py-isort-buffer))

;; pyimport
;; depedencies: s shut-up
(use-package pyimport
  :commands (pyimport-remove-unused pyimport-insert-missing))

;; yapfify
(use-package yapfify
  :commands (yapfify-region yapfify-buffer))

;; conda
;; dependencies: pythonic dash s f
(use-package conda
  :init (setq conda-anaconda-home (expand-file-name "~/anaconda3/"))
  :commands (conda-env-activate conda-env-deactivate))

(provide 'init-python)
;;; init-python.el ends here
