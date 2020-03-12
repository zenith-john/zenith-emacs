;;; init-keybinding.el --- more keybindings -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;; hydra
;; dependencies: lv

;; unset ',' keybinding for motion state.
(define-key evil-motion-state-map "," nil)

(general-define-key
 :states '(normal motion)
 :prefix ","
 "," 'switch-to-buffer
 "." 'find-file
 "g" 'magit-status
 "r" 'counsel-rg
 "s" 'snails)

(general-define-key
 :keymaps '(normal)
 "M-." 'xref-find-definitions)

(provide 'init-keybinding)
;;; init-keybinding ends here
