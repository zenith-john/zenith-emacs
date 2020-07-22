;;; init-evil.el --- evil configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; evil
;; dependencies: undo-tree goto-chg
(setq evil-want-integration t  ; This is optional since it's already set to t by default.
      evil-want-keybinding nil ; not loading keybindings
      evil-disable-insert-state-bindings t ; Use emacs's binding in insert state
      evil-want-C-d-scroll nil ; Use emacs's C-d
      evil-want-C-u-scroll nil ; Use emacs's C-u
      evil-want-C-i-jump t     ; Use vim's C-i
      evil-want-fine-undo t    ; Don not aggregate changes when exiting insert state
      evil-want-C-w-delete t   ; Use emacs's C-w
      evil-toggle-key ""       ; C-z not entering emacs state
      evil-respect-visual-line-mode t ; integration with visual-line-mode
      evil-move-beyond-eol t          ; emacs-like cursor movement
      evil-ex-substitute-global t     ; substitute global by default
      )

(require 'evil)

(evil-mode 1)

;; evil-anzu
;; dependencies: evil anzu
(require 'evil-anzu)
(global-anzu-mode 1)

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
;; dependencies: evil
(zenith/autoload
 '(evilnc-comment-operator
   evilnc-comment-or-uncomment-lines
   evilnc-comment-or-uncomment-paragraphs
   evilnc-comment-or-uncomment-to-the-line
   evilnc-copy-and-comment-lines
   evilnc-copy-and-comment-operator
   evilnc-copy-to-line)
 "evil-nerd-commenter")

;; evil-matchit
;; dependencies: evil
(require 'evil-matchit)
(setq evilmi-may-jump-by-percentage nil)
(global-evil-matchit-mode 1)

(provide 'init-evil)
;;; init-evil.el ends here
