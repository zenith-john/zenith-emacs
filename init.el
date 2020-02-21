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

(require 'cl-lib)
;; From https://emacs-china.org/t/topic/3931/2
(defun eh-hack-load-path ()
  ;; Delete buildin org's PATH
  (setq load-path
        (cl-remove-if
         #'(lambda (path)
             (string-match "lisp/org$" path))
         load-path))
  ;; Demove property lists to defeat cus-load and remove autoloads
  (mapatoms
   #'(lambda (sym)
       (let ((sym-name (symbol-name sym)))
         (when (string-match "^\\(org\\|ob\\|ox\\)-?" sym-name)
           (setplist sym nil)
           (when (autoloadp sym)
             (unintern sym)))))))

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(eh-hack-load-path)
(add-to-list 'load-path zenith-emacs-config-dir)
(add-subdirs-to-load-path zenith-emacs-extension-dir)

;; benchmark-init-el
(require 'benchmark-init)
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
