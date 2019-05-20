;;; init.el --- init file -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(defvar zenith-emacs-root-dir (file-truename "~/zenith-emacs/"))
(defvar zenith-emacs-extension-dir (expand-file-name "extensions/" zenith-emacs-root-dir))
(defvar zenith-emacs-config-dir (expand-file-name "config/" zenith-emacs-root-dir))
(defvar zenith-emacs-local-dir (expand-file-name "local/" zenith-emacs-root-dir))
(defvar zenith-file-name-handler-alist file-name-handler-alist "Remember the origin value of file-name-handler-alist")

(setq file-name-handler-alist nil)

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-to-list 'load-path zenith-emacs-config-dir)
(add-subdirs-to-load-path zenith-emacs-extension-dir)

(require 'benchmark-init-loaddefs)
(benchmark-init/activate)
(add-hook 'after-init-hook 'benchmark-init/deactivate)
(require 'config)

(add-hook 'after-init-hook (lambda ()(setq file-name-handler-alist zenith-file-name-handler-alist)))
(add-hook 'after-init-hook (lambda ()(setq gc-cons-threshold 16777216)))

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
