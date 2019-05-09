;;; init-keybinding.el --- more keybindings -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:
(use-package key-chord
  :config
  (key-chord-define-global ",," 'switch-to-buffer)
  (key-chord-define-global ",." 'find-file)
  (key-chord-mode 1))
 
(provide 'init-keybinding)
;;; init-keybinding ends here
