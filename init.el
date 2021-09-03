;; init.el --- init file -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(defvar zenith-emacs-root-dir user-emacs-directory)
(defvar zenith-emacs-extension-dir (expand-file-name "extensions/" zenith-emacs-root-dir))
(defvar zenith-emacs-config-dir (expand-file-name "config/" zenith-emacs-root-dir))
(defvar zenith-emacs-local-dir (expand-file-name "local/" zenith-emacs-root-dir))
(defvar zenith/wsl-system nil "Whether emacs starts in wsl. Some interaction with Windows are enabled")
(defvar zenith/enable-vterm t "Whether enable vterm module.")
(defvar zenith/enable-pyim t "Whether enable pyim and rime module.")
(defvar zenith/enable-posframe (display-graphic-p) "Whether enable posframe module.")
(defvar zenith/low-gc-cons-threshold (* 20 1024 1024))

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

  (eh-hack-load-path)
  ;; Load new org-mode rather than system one
  (add-to-list 'load-path zenith-emacs-config-dir)
  (add-subdirs-to-load-path zenith-emacs-extension-dir)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")

  (require 'benchmark-init)
  (require 'benchmark-init-modes)
  (benchmark-init/activate)
  (add-hook 'after-init-hook 'benchmark-init/deactivate)
  (require 'config)
  )

(setq gc-cons-threshold zenith/low-gc-cons-threshold)

(add-hook 'minibuffer-setup-hook 'zenith/temp-no-gc)
(add-hook 'minibuffer-exit-hook 'zenith/restore-gc)

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
