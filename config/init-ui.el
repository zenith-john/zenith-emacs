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
(when (daemonp)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (select-frame frame)
              (zenith/init-font))))

;; darkroom
(zenith/autoload '(darkroom-mode darkroom-tentative-mode) "darkroom")
(setq darkroom-text-scale-increase 1)

;; doom-themes
(require 'doom-themes)
(load-theme 'doom-one t)

;; doom-modeline
;; dependencies: all-the-icons shrink-path dash
;; all-the-icons depends on emacs-memoize
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

;; goto-line-preview
(autoload 'goto-line-preview "goto-line-preview")
(general-def [remap goto-line] 'goto-line-preview)

(provide 'init-ui)
;;; init-ui.el ends here
