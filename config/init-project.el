;;; init-project.el --- project manager -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; projectile
;; dependencies: pkg-info
(require 'projectile)

(setq projectile-cache-file (concat zenith-emacs-local-dir "projectile.cache")
      projectile-enable-caching t
      projectile-known-projects-file (concat zenith-emacs-local-dir "projectile.projects")
      projectile-require-project-root t
      ;; the following ignored files is not used
      ;; Because we use external tools to get files
      projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
      projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o")
      projectile-ignored-projects '("~/" "/tmp")
      projectile-kill-buffers-filter 'kill-only-files
      projectile-files-cache-expire 604800) ; expire after a week

(add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)
(projectile-mode +1)

(provide 'init-project)
;;; init-project.el ends here
