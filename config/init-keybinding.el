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
 ";" 'evilnc-comment-operator
 "\\" 'evilnc-copy-and-comment-operator
 "a" 'org-agenda
 "A" 'zenith/my-org-agenda
 "f" 'format-all-buffer
 "g" 'magit-status
 "r" 'counsel-rg
 "s" 'snails)

(general-define-key
 :keymaps '(normal motion)
 "M-." 'xref-find-definitions
 "C-w" 'zenith/hydra-window/body)

(general-define-key
 "M-;" 'evilnc-comment-or-uncomment-lines)

(provide 'init-keybinding)
;;; init-keybinding ends here
