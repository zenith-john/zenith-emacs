;;; init-keybinding.el --- more keybindings -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;; hydra
;; dependencies: lv
(use-package hydra)

(general-define-key
 :keymaps '(normal)
 :prefix ","
 "," 'snails
 "." 'find-file
 "g" 'magit-status
 "r" 'counsel-rg)

(provide 'init-keybinding)
;;; init-keybinding ends here
