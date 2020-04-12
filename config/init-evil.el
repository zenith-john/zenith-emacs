;;; init-evil.el --- evil configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; evil
;; dependencies: undo-tree goto-chg
(setq evil-want-integration t  ; This is optional since it's already set to t by default.
      evil-want-keybinding nil ; loading keybindings
      evil-disable-insert-state-bindings t ; Use emacs's binding in insert state
      evil-want-C-d-scroll nil ; Use emacs's C-d
      evil-want-C-u-scroll nil ; Use emacs's C-u
      evil-want-C-i-jump t     ; Use vim's C-i
      evil-want-fine-undo t    ; Don not aggregate changes when exiting insert state
      evil-want-C-w-delete t   ; Use emacs's C-w
      evil-toggle-key ""       ; C-z not entering emacs state
      )

(require 'evil)

(add-to-list 'evil-emacs-state-modes 'eaf-mode)
(add-to-list 'evil-emacs-state-modes 'snails-mode)

(evil-mode 1)

;; evil-collection
;; dependencies: evil
(require 'evil-collection)
(setq evil-collection-company-use-tng nil)
(evil-collection-init)

;; evil-surround
;; dependencies: evil
(require 'evil-surround)
(global-evil-surround-mode 1)

;; evil-nerd-commenter
;; depedencies: evil
(require 'evil-nerd-commenter)

;; evil-matchit
;; dependencies: evil
(require 'evil-matchit)
(setq evilmi-may-jump-by-percentage nil)
(global-evil-matchit-mode 1)

(provide 'init-evil)
;;; init-evil.el ends here
