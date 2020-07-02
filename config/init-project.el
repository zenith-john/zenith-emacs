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

;; Redefine `projectile-dir-files-alien' to ignore submodules searching by
;; default
(defvar projectile-include-subproject nil
  "Whether or not find file in subprojects too.")

(defun projectile-dir-files-alien (directory)
  "Get the files for DIRECTORY using external tools."
  (let ((vcs (projectile-project-vcs directory)))
    (cond
     ((eq vcs 'git)
      (nconc (projectile-files-via-ext-command directory (projectile-get-ext-command vcs))
             (when projectile-include-subproject (projectile-get-sub-projects-files directory vcs))))
     (t (projectile-files-via-ext-command directory (projectile-get-ext-command vcs))))))

;; counsel-projectile
;; better integration of ivy and projectile
(require 'counsel-projectile)

;; Switch between `counsel-projectile-find-file' and `find-file'
(defun zenith/toggle-projectile-and-normal ()
  (interactive)
  (if (projectile-project-root)
      (let ((old (ivy-state-caller ivy-last))
            new)
        (pcase old
         ('counsel-projectile-find-file
          (setq new 'counsel-find-file))
         ('counsel-find-file
          (setq new 'counsel-projectile-find-file))
         ('ivy-switch-buffer
          (setq new 'counsel-projectile-switch-to-buffer))
         ('counsel-projectile-switch-to-buffer
          (setq new 'ivy-switch-buffer)))
        (ivy-quit-and-run
          (let ((ivy-initial-inputs-alist `((t . ,ivy-text))))
            (funcall-interactively new))))
    (message "Cannot switch to other mode.")))

;; Find-file
(defun zenith/find-file ()
  (interactive)
  (if (projectile-project-root)
      (counsel-projectile-find-file)
    (counsel-find-file)))

;; Switch-buffer
(defun zenith/switch-buffer ()
  (interactive)
  (if (projectile-project-root)
      (counsel-projectile-switch-to-buffer)
    (ivy-switch-buffer)))

(add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook)
(projectile-mode +1)

(provide 'init-project)
;;; init-project.el ends here
