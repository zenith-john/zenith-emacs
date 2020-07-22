;; init.el --- init file -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(defvar zenith-emacs-root-dir (file-truename "~/zenith-emacs/"))
(defvar zenith-emacs-extension-dir (expand-file-name "extensions/" zenith-emacs-root-dir))
(defvar zenith-emacs-config-dir (expand-file-name "config/" zenith-emacs-root-dir))
(defvar zenith-emacs-local-dir (expand-file-name "local/" zenith-emacs-root-dir))

;; Do not load package
(setq initial-major-mode 'fundamental-mode
      package-enable-at-startup nil
      frame-inhibit-implied-resize t)

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(let (
      ;; Reduce GC to speed up initialization.
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      ;; Do not load major mode
      (file-name-handler-alist nil))

  (add-to-list 'load-path zenith-emacs-config-dir)
  (add-subdirs-to-load-path zenith-emacs-extension-dir)

  (require 'benchmark-init)
  (require 'benchmark-init-modes)
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
  (require 'config)
  )

;; To remove gc limit temporarily
(defun zenith/temp-no-gc ()
  (setq gc-cons-threshold most-positive-fixnum))

(defvar zenith/low-gc-cons-threshold (* 20 1024 1024))

;; Restore gc
(defun zenith/restore-gc ()
  (setq gc-cons-threshold zenith/low-gc-cons-threshold))

(zenith/restore-gc)

(add-hook 'minibuffer-setup-hook 'zenith/temp-no-gc)
(add-hook 'minibuffer-exit-hook 'zenith/restore-gc)

(add-hook 'minibuffer-setup-hook #'zenith/temp-no-gc)
(add-hook 'minibuffer-exit-hook #'zenith/restore-gc)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/home/zenith-john/Dropbox/task.org" "/home/zenith-john/Dropbox/idea.org" "/home/zenith-john/Dropbox/personal.org" "/home/zenith-john/Dropbox/vocal.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
