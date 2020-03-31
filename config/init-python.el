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

;; pyimport
;; depedencies: s shut-up
(zenith/autoload
 '(pyimport-remove-unused pyimport-insert-missing) "pyimport")

(defun zenith/company-sort-by-alphabet (candidates)
  (sort candidates #'string-lessp))

(defun zenith/python-mode-hook ()
  "Fine tune delay to have more smooth python editing"
  (setq-local company-transformers '(zenith/company-sort-by-alphabet)))

(add-hook 'python-mode-hook 'zenith/python-mode-hook)

(provide 'init-python)
;;; init-python.el ends here
