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
(load-theme 'doom-one t)

;; awesome-tray
(require 'awesome-tray)
(awesome-tray-mode 1)
(setq awesome-tray-active-modules
     '("evil" "location" "parent-dir" "buffer-name" "mode-name"))

(global-hl-line-mode 1)

;; goto-line-preview
(autoload 'goto-line-preview "goto-line-preview")
(general-def [remap goto-line] 'goto-line-preview)

(provide 'init-ui)
;;; init-ui.el ends here
