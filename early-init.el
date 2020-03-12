;;; early-init.el -*- lexical-binding: t; -*-

;; From https://github.com/hlissner/doom-emacs/
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.

;; Prevent the glimpse of un-styled Emacs by setting these early.
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))

;; One less file to load at startup
(setq site-run-file nil)

(provide 'early-init)
;;; early-init.el ends here
