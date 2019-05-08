;;; init-project.el --- project manager -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-add-known-project) ; TODO PR autoload upstream
  :init
  (setq projectile-cache-file (concat zenith-emacs-local-dir "projectile.cache")
        projectile-enable-caching (not noninteractive)
        projectile-known-projects-file (concat zenith-emacs-local-dir "projectile.projects")
        projectile-require-project-root t
        projectile-globally-ignored-files '(".DS_Store" "Icon
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
        projectile-ignored-projects '("~/" "/tmp")
        projectile-kill-buffers-filter 'kill-only-files
        projectile-files-cache-expire 604800) ; expire after a week
  :config
  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)
  (projectile-mode +1)

  (push ".project" projectile-project-root-files-bottom-up)
  (push (abbreviate-file-name zenith-emacs-local-dir) projectile-globally-ignored-directories))

(provide 'init-project)
;;; init-project.el ends here