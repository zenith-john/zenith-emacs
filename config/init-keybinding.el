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

(use-package hydra
  :config
  (require 'pretty-hydra)

  (pretty-hydra-define zenith/reading-mode
      (:foreign-keys run :title "Reading Mode" :quit-key "q")
    ("Move"
     (("h" backward-char "←")
      ("j" next-line "↓")
      ("k" previous-line "↑")
      ("l" forward-char "→")
      ("g" goto-line-preview)
      ("s" avy-goto-char)
      ("f" avy-goto-char-in-line))
     "Git Gutter"
     (("p" git-gutter:previous-hunk)
      ("n" git-gutter:next-hunk))))

  (general-def "M-g" 'zenith/reading-mode/body)

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

  (general-def "M-a" 'zenith/window-mode/body)

  (pretty-hydra-define zenith/sp-mode
      (:foreign-keys run :title "Smartparen Mode" :quit-key "q")
    ("Navigation"
     (("a" sp-beginning-of-sexp "Begin")
      ("e" sp-end-of-sexp "End")
      ("n" sp-down-sexp "Down")
      ("p" sp-up-sexp "Up")
      ("F" sp-forward-sexp "Forward Exp")
      ("B" sp-backward-sexp "Backward Exp")
      ("f" sp-forward-symbol "Forward Sym")
      ("b" sp-backward-symbol "Backward Sym"))
     "Warp"
     (("w" sp-mark-sexp "Warp")
      ("(" sp-wrap-round "()")
      ("[" sp-wrap-square "[]")
      ("{" sp-wrap-curly "{}"))
     "Unwarp"
     (("u" sp-unwrap-sexp "Unwrap")
      ("U" sp-backward-unwrap-sexp "Unwrap Back"))
     "Slurp and Barf"
     (("<right>" sp-forward-slurp-sexp "Slurp right")
      ("<left>" sp-backward-slurp-sexp "Slurp left")
      ("<down>" sp-forward-barf-sexp "Barf right")
      ("<up>" sp-backward-barf-sexp "Barf left"))
     "Manipulation"
     (("t" sp-transpose-sexp "Transpose")
      ("k" sp-kill-sexp "Kill sexp")
      ("K" sp-kill-hybrid-sexp "Kill hybrid")
      ("M-k" sp-backward-kill-sexp "Kill backward"))))

  (general-def "M-\\" 'zenith/sp-mode/body))

(defvar symbol-row-p nil)

(defvar zenith/translation-table
  '(("0" "\)")
    ("1" "!")
    ("2" "@")
    ("3" "#")
    ("4" "$")
    ("5" "%")
    ("6" "^")
    ("7" "&")
    ("8" "*")
    ("9" "(")))

(defun zenith/trans-key (key1 key2)
  (define-key key-translation-map (kbd key1) (kbd key2)))

(defun zenith/swap-key (key1 key2)
  (zenith/trans-key key1 key2)
  (zenith/trans-key key2 key1))

(defun zenith/toggle-number-symbol-row ()
  "Make number row symbol row!"
  (interactive)
  (if symbol-row-p
      (setq key-translation-map (make-sparse-keymap)
            symbol-row-p nil)
    (setq symbol-row-p t)
    (dolist (keys zenith/translation-table)
      (apply #'zenith/swap-key keys))))

(provide 'init-keybinding)
;;; init-keybinding ends here
