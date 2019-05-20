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

(with-eval-after-load 'hydra
  (defhydra hydra-M-g (:color pink
                       :hint nil)
    "
   _k_        ^^_g_: goto-line      _p_: previous git gutter
_h_     _l_     _s_: goto-char      _n_: next git gutter
   _j_        ^^_f_: char in line   _q_: quit
"
    ("h" backward-char)
    ("l" forward-char)
    ("k" previous-line)
    ("j" next-line)
    ("g" goto-line-preview)
    ("s" avy-goto-char)
    ("f" avy-goto-char-in-line)
    ("p" git-gutter:previous-hunk)
    ("n" git-gutter:next-hunk)
    ("q" nil))
  (general-def "M-g" 'hydra-M-g/body))

(provide 'init-keybinding)
;;; init-keybinding ends here
