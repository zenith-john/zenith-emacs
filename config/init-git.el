;;; init-git.el --- git configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(use-package magit)

(use-package git-gutter
  :config
  (require 'git-gutter-fringe)
  ;; Configuration from https://github.com/hlissner/doom-emacs/ git-gutter module
  (fringe-mode '4)
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  (global-git-gutter-mode +1))

(provide 'init-git)
;;; init-git.el ends here
