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
 :states '(normal motion)
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
 "R"     'rg-project
 "s"     'snails)

(general-define-key
 :states '(normal motion)
 "gs"    'avy-goto-char
 "r"     'counsel-recentf
 "R"     'rg-dwim-project-dir
 "M-."   'xref-find-definitions
 "C-w"   'zenith/hydra-window/body)

(general-define-key
 [remap goto-line]    'goto-line-preview
 [remap list-buffers] 'ibuffer
 [tab]                yas-maybe-expand
 "C-."                'yas-insert-snippet
 "C-x C-j"            'company-complete-common
 "M-;"                'evilnc-comment-or-uncomment-lines
 "M-/"                'pyim-convert-string-at-point
 "M-o"                'ace-window)

(general-define-key
 :keymaps 'company-active-map
 "M-RET" (lambda ()
           (interactive)
           (company-abort)
           (newline-and-indent))
 [tab]    yas-maybe-expand)

(general-define-key
 :keymaps 'isearch-mode-map
 [escape] 'isearch-abort)

(general-define-key
 :states  '(normal motion)
 :keymaps 'org-agenda-mode-map
 "a"      'org-attach
 "o"      'org-agenda-attach-open)

;; I prefer C-c C-c over C-c ' (more consistent)
(general-define-key
 :keymaps  'org-src-mode-map
 "C-c C-c" 'org-edit-src-exit)

;; Exchange position of { and [ in latex mode to reduce the injury of the finger
(general-define-key
 :keymaps 'LaTeX-mode-map
 "、"     (lambda ()(interactive)(self-insert-command 1 ?\\))
 "C-*"    'LaTeX-star-environment-dwim
 "["      (lambda ()(interactive)(self-insert-command 1 ?{))
 "{"      (lambda ()(interactive)(self-insert-command 1 ?\[))
 )

;; I don't know why, but the ivy-mode set <escape> to minibuffer-keyboard-quit
;; and overwrite the keybindings in snails-mode-map.
(general-define-key
 :keymaps 'ivy-mode-map
 [escape] nil)

;; Use arrow to move and <escape> to quit.
(general-define-key
 :keymaps   'snails-mode-map
 "<down>"   'snails-select-next-item
 "<up>"     'snails-select-prev-item
 "<escape>" 'snails-quit)

(general-define-key
 :keymaps                               'ivy-mode-map
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

(provide 'init-keybinding)
;;; init-keybinding ends here
