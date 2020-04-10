;;; init-keybinding.el --- more keybindings -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;; hydra
;; dependencies: lv
(require 'hydra)
(defhydra zenith/hydra-window (:foreign-keys warn)
  ""
  ("=" evil-window-increase-height)
  ("-" evil-window-decrease-height)
  ("s" evil-window-split)
  ("v" evil-window-vsplit)
  ("<" evil-window-decrease-width)
  (">" evil-window-increase-width)
  ("+" balance-windows)
  ("u" winner-undo)
  ("r" winner-redo)
  ("q" nil :color blue))

;; unset ',' keybinding for motion state.
(define-key evil-motion-state-map "," nil)

(general-define-key
 :states '(normal motion)
 :prefix ","
 "," 'switch-to-buffer
 "." 'find-file
 "g" 'magit-status
 "r" 'counsel-rg
 "f" 'format-all-buffer
 "s" 'snails
 "a" 'org-agenda
 "A" 'zenith/my-org-agenda)

(general-define-key
 :keymaps '(normal motion)
 "M-." 'xref-find-definitions
 "C-w" 'zenith/hydra-window/body)

(provide 'init-keybinding)
;;; init-keybinding ends here
