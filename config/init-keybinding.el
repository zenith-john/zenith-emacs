;;; init-keybinding.el --- more keybindings -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:
;; I should introduce the structure of my keybindings. Thanks to powerful
;; general package, all the evil mode can use "," as their leader key and in
;; insert mode use C-, instead. Also I map my Shift_R mapped to <menu> by xcape.
;; And <menu> works as another leader key for mode specific commands.

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
  ("j" windmove-down)
  ("k" windmove-up)
  ("h" windmove-left)
  ("l" windmove-right)
  ("q" nil :color blue))

;; unset ',' keybinding for motion state.
(define-key evil-motion-state-map "," nil)

(general-define-key
 :states '(normal motion visual insert)
 :prefix ","
 :non-normal-prefix "<menu> ,"
 :prefix-command 'zenith/leader-map
 ","     'switch-to-buffer
 "."     'find-file
 ";"     'evilnc-comment-operator
 "\\"    'evilnc-copy-and-comment-operator
 "="     'er/expand-region
 "a"     'org-agenda
 "A"     'zenith/my-org-agenda
 "c"     'org-capture
 "f"     'format-all-buffer
 "g"     'magit-status
 "o"     'ace-window
 "r"     'counsel-recentf
 "R"     'rg-my-project
 "s"     'save-buffer
 "t"     'zenith/vterm-toggle
 "1"     'winum-select-window-1
 "2"     'winum-select-window-2
 "3"     'winum-select-window-3
 "4"     'winum-select-window-4
 "5"     'winum-select-window-5
 "6"     'winum-select-window-6
 "7"     'winum-select-window-7
 "8"     'winum-select-window-8
 "9"     'winum-select-window-9
 "0"     'delete-window
 "'"     'delete-other-windows
 "\\"    'evil-window-vsplit
 "-"     'evil-window-split
 )

;; Use remap instead of direct key to reduce conflicts with other package name
;; in particular evil-collection.
(general-define-key
 :states                           '(normal motion visual)
 "gs"                              'avy-goto-char
 "s"                               (general-simulate-key ('evil-ex "%s/"))
 "S"                               'swiper
 ";"                               'evil-ex
 ":"                               'evil-repeat-find-char
 "M-."                             'xref-find-definitions
 "C-v"                             'nil
 "M-v"                             'evil-visual-block
 "C-e"                             'nil
 "C-t"                             'nil
 "C-w"                             'zenith/hydra-window/body
 [remap evil-replace]              'zenith/jump
 [remap evil-paste-pop]            'counsel-yank-pop
 [remap isearch-forward]           'evil-search-forward
 [remap isearch-backward]          'evil-search-backward)

(general-define-key
 [remap goto-line]             'goto-line-preview
 [remap list-buffers]          'ibuffer
 "C-."                         'yas-insert-snippet
 "C-r"                         'evil-search-backward
 "C-v"                         'yank
 "C-x C-j"                     'company-complete-common
 "C-x C-s"                     (lambda ()(interactive)(message "Don't use C-x C-s to save, it hurts."))
 "C-z"                         'undo
 "M-;"                         'evilnc-comment-or-uncomment-lines
 "M-/"                         'zenith/rime-convert-string-at-point
 "M-o"                         'ace-window
 "M-d"                         'ace-delete-window
 [menu]                        'nil
 "<menu> <menu>"               'special-char-mode
 [remap backward-kill-word]    'zenith/delete-word-or-space
 [remap xref-pop-marker-stack] 'evil-jump-backward
 [remap fill-paragraph]        'zenith/fill-and-indent-region
 [remap beginning-of-buffer]   'evil-goto-first-line
 [remap end-of-buffer]         'evil-goto-line)

(general-define-key
 :states    '(insert emacs)
 "M-h"      'backward-char
 "M-l"      'forward-char
 "M-j"      'backward-delete-char
 "M-k"      'evil-normal-state)

(general-define-key
 :prefix "C-c"
 "1" (general-simulate-key "C-c !")
 "2" (general-simulate-key "C-c @")
 "3" (general-simulate-key "C-c #")
 "4" (general-simulate-key "C-c $")
 "5" (general-simulate-key "C-c %")
 "6" (general-simulate-key "C-c ^")
 "7" (general-simulate-key "C-c &")
 "8" (general-simulate-key "C-c *")
 "9" (general-simulate-key "C-c (")
 "0" (general-simulate-key "C-c )"))

(general-define-key
 :keymaps 'company-active-map
 "M-k"   'company-abort
 "M-RET" (lambda ()
           (interactive)
           (company-abort)
           (newline-and-indent))
 "M-TAB" 'counsel-company)

(general-define-key
 :keymaps 'undo-tree-map
 "C-r" nil)

;; Use ivy and counsel to replace ordinary maps
(general-define-key
 :keymaps                              'ivy-mode-map
 [remap switch-to-buffer]              'zenith/switch-buffer
 [remap switch-to-buffer-other-window] 'ivy-switch-buffer-other-window
 [remap imenu-anywhere]                'ivy-imenu-anywhere
 [remap apropos]                       'counsel-apropos
 [remap bookmark-jump]                 'counsel-bookmark
 [remap describe-face]                 'counsel-faces
 [remap describe-function]             'helpful-callable
 [remap describe-variable]             'helpful-variable
 [remap describe-bindings]             'helpful-key
 [remap set-variable]                  'counsel-set-variable
 [remap find-file]                     'zenith/find-file
 [remap find-library]                  'counsel-find-library
 [remap info-lookup-symbol]            'counsel-info-lookup-symbol
 [remap imenu]                         'counsel-imenu
 [remap recentf-open-files]            'counsel-recentf
 [remap org-capture]                   'counsel-org-capture
 [remap swiper]                        'counsel-grep-or-swiper
 "M-y"                                 'counsel-yank-pop
 "M-p"                                 'counsel-projectile-switch-project)

;; Use escape and M-k to escape everywhere.
(general-define-key
 :keymaps 'isearch-mode-map
 "M-k"    'isearch-abort
 [escape] 'isearch-abort)

(general-define-key
 :keymaps 'evil-ex-search-keymap
 "M-k" 'keyboard-escape-quit)

(general-define-key
 :keymaps 'ivy-minibuffer-map
 "M-a"    'ivy-read-action
 "M-m"    'ivy-mark
 "M-u"    'ivy-unmark
 "M-k"    'keyboard-escape-quit
 [menu]   'zenith/toggle-projectile-and-normal
 "M-h"    'zenith/toggle-projectile-and-normal
 [escape] 'keyboard-escape-quit)

(general-define-key
 :keymaps 'minibuffer-local-map
 [escape] 'minibuffer-keyboard-quit)

;; don't bother to expand in expansion by tab.
(general-define-key
 :keymaps 'yas-keymap
 [(tab)]  'yas-next-field
 "TAB"    'yas-next-field)

;; keymaps about major-mode specific keybindings
;; emacs-lisp-mode
(general-define-key
 :keymaps 'emacs-lisp-mode-map
 :prefix "<menu>"
 "e" 'eval-last-sexp
 "[" 'backward-sexp
 "]" 'forward-sexp)

(general-define-key
 :keymaps   'prog-mode-map
 :states    '(emacs insert)
 "<return>" 'newline-dwim)

;; Org-mode
(general-define-key
 :states  '(normal motion)
 :keymaps 'org-agenda-mode-map
 "a"      'org-attach
 "o"      'org-agenda-attach-open)

(general-define-key
 :keymaps 'org-mode-map
 "C-l" 'org-super-links-link
 "M-r" 'zenith/search-id-reverse-link)

;; I prefer C-c C-c over C-c ' (more consistent)
(general-define-key
 :keymaps  'org-src-mode-map
 "C-c C-c" 'org-edit-src-exit)

(general-define-key
 :keymaps 'org-mode-map
 :prefix [menu]
 "b" 'org-insert-structure-template
 "c" 'org-ctrl-c-ctrl-c
 "d" 'org-deadline
 "e" 'org-export-dispatch
 "s" 'org-schedule
 "t" 'org-todo
 "," 'org-priority
 "." 'org-time-stamp
 )

;; LaTeX-mode
;; Exchange position of { and [ in latex mode to reduce the injury of the finger
(general-define-key
 :keymaps 'LaTeX-mode-map
 "、"                     (lambda ()(interactive)(self-insert-command 1 ?\\))
 "C-c *"                  'LaTeX-star-environment-dwim
 "C-c ]"                  'zenith/cycle-equation
 "j"                      'zenith/latex-magic-j
 "["                      'zenith/latex-magic-bracket
 "-"                      'zenith/latex-magic-underscore
 "\""                     'zenith/latex-insert-quote
 [remap reftex-citation]  'ivy-bibtex
 [remap reftex-reference] 'counsel-reftex-labels
 "M-9"                    (lambda ()(interactive)
                            (self-insert-command 1 ?\\)
                            (self-insert-command 1 ?\())
 "M-["                    (lambda ()(interactive)
                            (self-insert-command 1 ?\\)
                            (self-insert-command 1 ?\[))
 "M-RET"                  'latex-insert-item)

(general-define-key
 :keymaps 'LaTeX-mode-map
 :prefix [f9]
 :prefix-command 'LaTeX-prefix-command
 :prefix-map 'LaTeX-prefix-map
 "a" 'zenith/latexmk-compile
 "c" 'TeX-command-master
 "e" 'LaTeX-environment
 "s" 'LaTeX-section
 "w" 'zenith/latex-watch
 "9" 'reftex-label
 "0" 'counsel-reftex-labels
 "v" 'TeX-view
 "[" 'reftex-citation
 "m" 'TeX-insert-macro)

(with-eval-after-load 'latex
  (general-define-key
   :keymaps    'LaTeX-mode-map
   :states     '(normal insert)
   [menu]      LaTeX-math-keymap
   "M-h"       'LaTeX-prefix-command
   "M-<left>"  'zenith/left-cell-fallback
   "M-<right>" 'zenith/right-cell-fallback
   "M-<up>"    'zenith/up-cell-fallback
   "M-<down>"  'zenith/down-cell-fallback))

(general-define-key
 :states '(normal motion)
 :keymaps 'inferior-ess-r-mode-map
 "," 'zenith/leader-map)

(provide 'init-keybinding)
;;; init-keybinding ends here
