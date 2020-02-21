;;; init-keybinding.el --- more keybindings -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:
(use-package hydra)

(general-define-key
 :keymaps 'normal
 :prefix ","
 "," 'switch-to-buffer
 "." 'find-file
 "S" 'magit-stash-both
 "s" 'magit-stage-this-file)

(provide 'init-keybinding)
;;; init-keybinding ends here
