;;; init-evil.el --- evil configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; evil
;; dependencies: undo-tree goto-chg
(use-package evil
  :init
  (setq evil-want-integration t  ; This is optional since it's already set to t by default.
        evil-want-keybinding nil ; loading keybindings
        evil-disable-insert-state-bindings t ; Use emacs's binding in insert state
        evil-want-C-d-scroll nil ; Use emacs's C-d
        evil-want-C-u-scroll nil ; Use emacs's C-u
        evil-want-fine-undo t  ; Don not aggregate changes when exiting insert state
        evil-want-C-w-delete t ; Use emacs's C-w
        evil-toggle-key "" ; C-z not entering emacs state
        )
  :config
  (add-to-list 'evil-emacs-state-modes 'eaf-mode)
  (evil-mode 1))

;; evil-collection
;; dependencies: evil
(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  :config
  (evil-collection-init))

;; evil-matchit
;; dependencies: evil
(use-package evil-matchit
  :after evil
  :init
  (setq evilmi-may-jump-by-percentage nil)
  :config
  (global-evil-matchit-mode 1))

(provide 'init-evil)
;;; init-evil.el ends here
