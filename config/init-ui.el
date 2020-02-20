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

(add-hook 'after-init-hook 'zenith/init-font)

(use-package darkroom
  :commands (darkroom-mode darkroom-tentative-mode)
  :init
  (setq darkroom-text-scale-increase 1))

(require 'doom-themes)
(load-theme 'doom-one t)

(require 'doom-modeline)

;; Use minimal height to avoid the overlap of org-set-tags-command.
(setq doom-modeline-height 1)

;; Redefine doom-modeline--font-heigth
(defun doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (let ((height (face-attribute 'mode-line :height)))
    (round (cond ((integerp height) (/ height 10))
                         ((floatp height) (* height (frame-char-height)))
                         (t (frame-char-height))))))

(doom-modeline-mode 1)

(global-hl-line-mode 1)

(use-package goto-line-preview
  :commands (goto-line-preview)
  :init
  (general-def [remap goto-line] 'goto-line-preview))

(provide 'init-ui)
;;; init-ui.el ends here
