;;; init-keybinding.el --- more keybindings -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:
(use-package hydra
  :config
  (require 'pretty-hydra)

  (pretty-hydra-define zenith/window-mode
      (:foreign-keys warn :title "Window Mode" :quit-key "q")
    ("Action"
     (("TAB" other-window "Switch")
      ("x" ace-delete-window "Delete")
      ("m" ace-delete-other-windows "Maximize")
      ("s" ace-swap-window "Swap")
      ("a" ace-select-window "Select")
      ("d" delete-window "Kill"))
     "Resize"
     (("o" enlarge-window "Enlarge")
      ("i" shrink-window "Shrink")
      ("O" enlarge-window-horizontally "Enlarge horizontally")
      ("I" shrink-window-horizontally "Shrink horizontally")
      ("n" balance-window "Balance")
      ("f" toggle-frame-fullscreen "Toggle fullscreen"))
     "Split"
     (("|" split-window-right "Split right")
      ("b" split-window-horizontally-instead "Split left")
      ("-" split-window-below "Split below")
      ("v" split-window-vertically-instead "Split up"))))

  (general-def "M-a" 'zenith/window-mode/body))

(general-define-key
 :keymaps 'normal
 :prefix ","
 "," 'switch-to-buffer
 "." 'find-file
 "S" 'magit-stash-both
 "s" 'magit-stage-this-file)

(provide 'init-keybinding)
;;; init-keybinding ends here
