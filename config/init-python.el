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
(autoload 'pipenv-mode "pipenv-mode")
  (add-hook 'python-mode-hook 'pipenv-mode)
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended)

;; emacs-python-pytest
;; dependencies: dash dash-functional magit-popup projectile s
(zenith/autoload '(
   python-pytest
   python-pytest-file
   python-pytest-file-dwim
   python-pytest-function
   python-pytest-function-dwim
   python-pytest-last-failed
   python-pytest-repeat
   python-pytest-popup
   ) "python-pytest")

;; pyvenv
(zenith/autoload
 '(pyvenv-activate
   pyvenv-workon) "pyvenv")

;; py-isort
(autoload 'py-isort-buffer "py-isort")

;; pyimport
;; depedencies: s shut-up
(zenith/autoload
 '(pyimport-remove-unused pyimport-insert-missing) "pyimport")

;; yapfify
(zenith/autoload
  '(yapfify-region yapfify-buffer) "yapfify")

;; conda
;; dependencies: pythonic dash s f
(setq conda-anaconda-home (expand-file-name "~/anaconda3/"))
(zenith/autoload '(conda-env-activate conda-env-deactivate) "conda")

(provide 'init-python)
;;; init-python.el ends here
