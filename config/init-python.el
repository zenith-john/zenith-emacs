;;; init-python.el --- python configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; pipenv
;; dependencies: pyvenv s f
(autoload 'pipenv-mode "pipenv")
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

;; pyenv-mode
;; dependencies: pythonic

(zenith/delay-load
 (lambda ()
   (require 'pyenv-mode)

   ;; It is worth noting that emacs read $HOME/.profile to load environment variables.
   ;; From http://rakan.me/emacs/python-dev-with-emacs-and-pyenv/
   (defun zenith/pyenv-init-global ()
     "Initialize pyenv version to the global one"
     (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
       (pyenv-mode-set global-pyenv)))

   (zenith/pyenv-init-global)))

;; pyimport
;; depedencies: s shut-up
(zenith/autoload
 '(pyimport-remove-unused pyimport-insert-missing) "pyimport")

(provide 'init-python)
;;; init-python.el ends here
