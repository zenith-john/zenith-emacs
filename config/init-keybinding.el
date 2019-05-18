;;; init-keybinding.el --- more keybindings -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:
(with-eval-after-load 'general
  (general-def ","
    (general-key-dispatch 'self-insert-command
      :timeout 0.2
      "," 'switch-to-buffer
      "." 'find-file)))

(provide 'init-keybinding)
;;; init-keybinding ends here
