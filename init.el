;;; init.el --- init file -*- lexical-binding: t; -*-

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
  (require 'config))

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
