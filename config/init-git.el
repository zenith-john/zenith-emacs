;;; init-git.el --- git configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; magit
;; dependencies: async dash git-commit transient with-editor
(if (executable-find "git")
    (progn
      (zenith/autoload  '(magit-status
                          magit-stage-file
                          magit-unstage-file
                          magit-find-file
                          magit-init
                          magit-dispatch
                          magit-stash-both) "magit")

      (defun magit-stage-this-file ()
        "Stage current buffer for magit"
        (interactive)
        (magit-stage-file (buffer-file-name)))

      ;; emacs-git-gitter
      (require 'git-gutter)
      ;; emacs-git-gutter-fringe
      ;; dependencies: git-gutter fringe-helper
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
      (global-git-gutter-mode +1)

      ;; git-timemachine
      ;; dependencies: transient
      (zenith/autoload
       '(git-timemachine
         git-timemachine-toggle) "git-timemachine")
      (evil-ex-define-cmd "git"        #'magit-status)
      (evil-ex-define-cmd "gs[tage]"   #'magit-stage)
      (evil-ex-define-cmd "gu[nstage]" #'magit-unstage)
      (evil-ex-define-cmd "gb[lame]"   #'magit-blame)
      (evil-ex-define-cmd "gp[rev]"    #'git-gutter:previous-hunk)
      (evil-ex-define-cmd "gn[ext]"    #'git-gutter:next-hunk)
      (evil-ex-define-cmd "gr[evert]"  #'git-gutter:revert-hunk))
  (defun magit-status ()
    (interactive)
    (message "Program git is not found.")))

(provide 'init-git)
;;; init-git.el ends here
