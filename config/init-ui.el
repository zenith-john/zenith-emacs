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
(load-theme 'doom-palenight t)

(require 'doom-themes-ext-org)
(doom-themes-org-config)

(require 'doom-modeline)
;; the height has to be 1 to use org-set-tags-command
(setq doom-modeline-height 25)
(doom-modeline-mode 1)
(setq inhibit-compacting-font-caches t)


;; Redefine `doom-modeline-redisplay' to ignore `doom-modeline--size-hacked-p'
;; to fix the problem caused by reuse of some buffer, for example *Org Tags*
(defun zenith/doom-modeline-always-redisplay ()
  "Check whether this buffer should always display"
  (or (eq (buffer-name) "Org tags")))

(defun doom-modeline-redisplay (&rest _)
  "Call `redisplay' to trigger mode-line height calculations.

Certain functions, including e.g. `fit-window-to-buffer', base
their size calculations on values which are incorrect if the
mode-line has a height different from that of the `default' face
and certain other calculations have not yet taken place for the
window in question.

These calculations can be triggered by calling `redisplay'
explicitly at the appropriate time and this functions purpose
is to make it easier to do so.

This function is like `redisplay' with non-nil FORCE argument.
It accepts an arbitrary number of arguments making it suitable
as a `:before' advice for any function.  If the current buffer
has no mode-line or this function has already been calle in it,
then this function does nothing."
  (when (and (bound-and-true-p doom-modeline-mode)
             mode-line-format
             (not doom-modeline--size-hacked-p))
    (redisplay t)
    (unless (zenith/doom-modeline-always-redisplay)
      (setq doom-modeline--size-hacked-p t))))

;; Redefine `hl-line-highlight' to disable highlight line when selection is
;; active.
(defun hl-line-highlight ()
  "Activate the Hl-Line overlay on the current line."
  (if
      (and hl-line-mode	; Might be changed outside the mode function.
           (not (region-active-p)))
      (progn
        (unless hl-line-overlay
          (setq hl-line-overlay (hl-line-make-overlay))) ; To be moved.
        (overlay-put hl-line-overlay
                     'window (unless hl-line-sticky-flag (selected-window)))
	(hl-line-move hl-line-overlay))
    (hl-line-unhighlight)))

(global-hl-line-mode 1)

;; goto-line-preview
(autoload 'goto-line-preview "goto-line-preview")

(provide 'init-ui)
;;; init-ui.el ends here
