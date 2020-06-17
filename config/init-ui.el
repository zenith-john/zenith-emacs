;;; init-ui.el --- ui configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(defvar zenith-font (font-spec :family "Iosevka Term SS09" :size 16 :weight 'semi-bold))
(defvar zenith-unicode-font (font-spec :family "Sarasa Term SC" :weight 'bold))
(setq inhibit-x-resources t)

(defun zenith/init-font ()
  (add-to-list 'default-frame-alist `(font . ,(font-xlfd-name zenith-font)))
  (set-fontset-font t 'unicode zenith-unicode-font nil 'prepend))

(zenith/init-font)

;; darkroom
(zenith/autoload '(darkroom-mode darkroom-tentative-mode) "darkroom")
(setq darkroom-text-scale-increase 1)

;; doom-themes
(require 'doom-themes)
(load-theme 'doom-material t)

(require 'doom-themes-ext-org)
(doom-themes-org-config)

(require 'doom-modeline)
(setq doom-modeline-height 1)
(doom-modeline-mode 1)


(global-hl-line-mode 1)

;; goto-line-preview
(autoload 'goto-line-preview "goto-line-preview")

(provide 'init-ui)
;;; init-ui.el ends here
