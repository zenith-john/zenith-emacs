;;; init-ui.el --- ui configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(defvar zenith-font (font-spec :family "Iosevka Term SS09" :size 16 :weight 'semi-bold))
(defvar zenith-unicode-font (font-spec :family "Sarasa Term SC" :weight 'bold))

(defun zenith/init-font ()
  (add-to-list 'default-frame-alist `(font . ,(font-xlfd-name zenith-font)))
  (set-fontset-font t nil zenith-unicode-font nil 'append))

(add-hook 'after-init-hook 'zenith/init-font)

(use-package darkroom
  :commands (darkroom-mode darkroom-tentative-mode)
  :init
  (setq darkroom-text-scale-increase 1))

(require 'doom-themes)
(load-theme 'doom-one t)

(doom-themes-org-config)

(require 'doom-modeline)
(doom-modeline-mode 1)

(global-hl-line-mode 1)

(use-package goto-line-preview
  :commands (goto-line-preview)
  :init
  (general-def [remap goto-line] 'goto-line-preview))

(provide 'init-ui)
;;; init-ui.el ends here
