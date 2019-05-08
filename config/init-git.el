;;; init-git.el --- git configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(use-package magit)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

(provide 'init-git)
;;; init-git.el ends here
