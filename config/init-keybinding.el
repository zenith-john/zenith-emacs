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
 :states '(normal motion visual)
 :prefix ","
 ","     'switch-to-buffer
 "."     'find-file
 ";"     'evilnc-comment-operator
 "\\"    'evilnc-copy-and-comment-operator
 "="     'er/expand-region
 "a"     'org-agenda
 "A"     'zenith/my-org-agenda
 "f"     'format-all-buffer
 "g"     'magit-status
 "r"     'counsel-rg
 "R"     'rg-project)

;; Use remap instead of direct key to reduce conflicts with other package name
;; in particular evil-collection.
(general-define-key
 :states                           '(normal motion visual)
 "gs"                              'avy-goto-char
 "gr"                              'counsel-recentf
 "gR"                              'rg-dwim-project-dir
 "M-."                             'xref-find-definitions
 "C-v"                             'nil
 "M-v"                             'evil-visual-block
 "C-e"                             'nil
 "C-w"                             'zenith/hydra-window/body
 [remap evil-search-next]          'zenith/ctrlf-next-match
 [remap evil-search-previous]      'zenith/ctrlf-previous-match
 [remap evil-replace]              'zenith/jump
 [remap evil-paste-after]          'yank
 [remap evil-paste-before]         'counsel-yank-pop
 [remap evil-search-forward]       'ctrlf-forward-regexp
 [remap evil-search-backward]      'ctrlf-backward-regexp)

(general-define-key
 [remap goto-line]             'goto-line-preview
 [remap list-buffers]          'ibuffer
 "C-."                         'yas-insert-snippet
 "C-f"                         'isearch-forward
 "C-r"                         'isearch-backward
 "C-x C-j"                     'company-complete-common
 "M-;"                         'evilnc-comment-or-uncomment-lines
 "M-/"                         'pyim-convert-string-at-point
 "M-o"                         'ace-window
 "M-k"                         'ace-delete-window
 "M-TAB"                       'counsel-company
 "<menu>"                      'nil
 [remap backward-kill-word]    'zenith/delete-word-or-space
 [remap xref-pop-marker-stack] 'evil-jump-backward
 [remap fill-paragraph]        'zenith/fill-and-indent-region)

(general-define-key
 :prefix "<menu>"
 "<menu>" 'special-char-mode)

(general-define-key
 :prefix "C-x"
 "f" 'find-file
 "s" 'save-buffer)

(general-define-key
 :keymaps 'emacs-lisp-mode-map
 "C-x e" 'eval-last-sexp)

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
 "M-RET" (lambda ()
           (interactive)
           (company-abort)
           (newline-and-indent))
 "M-TAB" 'counsel-company)

(general-define-key
 :keymaps 'isearch-mode-map
 [escape] 'isearch-abort)

(general-define-key
 :keymaps 'undo-tree-map
 "C-r" nil)

(general-define-key
 :states  '(normal motion)
 :keymaps 'org-agenda-mode-map
 "a"      'org-attach
 "o"      'org-agenda-attach-open)

(general-define-key
 :keymaps 'org-mode-map
 "C-l" 'zenith/org-insert-link-by-id
 "M-r" 'zenith/search-id-reverse-link)

;; I prefer C-c C-c over C-c ' (more consistent)
(general-define-key
 :keymaps  'org-src-mode-map
 "C-c C-c" 'org-edit-src-exit)

;; Exchange position of { and [ in latex mode to reduce the injury of the finger
(general-define-key
 :keymaps 'LaTeX-mode-map
 "、"     (lambda ()(interactive)(self-insert-command 1 ?\\))
 "C-c *"  'LaTeX-star-environment-dwim
 "C-c ]"  'zenith/cycle-equation
 "["      'zenith/latex-magic-bracket
 "-"      'zenith/magic-underscore
 )

(general-define-key
 :keymaps                              'ivy-mode-map
 [remap switch-to-buffer]              'ivy-switch-buffer
 [remap switch-to-buffer-other-window] 'ivy-switch-buffer-other-window
 [remap imenu-anywhere]                'ivy-imenu-anywhere
 [remap apropos]                       'counsel-apropos
 [remap bookmark-jump]                 'counsel-bookmark
 [remap describe-face]                 'counsel-faces
 [remap describe-function]             'counsel-describe-function
 [remap describe-variable]             'counsel-describe-variable
 [remap describe-bindings]             'counsel-descbinds
 [remap set-variable]                  'counsel-set-variable
 [remap find-file]                     'counsel-find-file
 [remap find-library]                  'counsel-find-library
 [remap info-lookup-symbol]            'counsel-info-lookup-symbol
 [remap imenu]                         'counsel-imenu
 [remap recentf-open-files]            'counsel-recentf
 [remap org-capture]                   'counsel-org-capture
 [remap swiper]                        'counsel-grep-or-swiper
 "M-y"                                 'counsel-yank-pop
 "C-h f"                               'helpful-callable
 "C-h v"                               'helpful-variable
 "C-h k"                               'helpful-key)

(general-define-key
 :keymaps 'ivy-minibuffer-map
 [escape] 'keyboard-escape-quit)

(general-define-key
 :keymaps 'minibuffer-local-map
 [escape] 'minibuffer-keyboard-quit)

;; don't bother to expand in expansion by tab.
(general-define-key
 :keymaps 'yas-keymap
 [(tab)]  'yas-next-field
 "TAB"    'yas-next-field)

(general-define-key
 :keymaps 'LaTeX-mode-map
 :prefix "<menu>"
 "a" 'TeX-command-run-all
 "c" 'TeX-command-master
 "e" 'LaTeX-environment
 "s" 'LaTeX-section
 "9" 'reftex-label
 "0" 'reftex-reference
 "[" 'reftex-citation
 "]" 'zenith/cycle-equation)

(provide 'init-keybinding)
;;; init-keybinding ends here
