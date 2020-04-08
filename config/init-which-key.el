;;; init-which-key.el --- which-key configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; emacs-which-key
(require 'which-key)
(setq which-key-sort-order #'which-key-prefix-then-key-order
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-min-display-lines 6
      which-key-side-window-slot -10)
;; general improvements to which-key readability
(which-key-setup-side-window-bottom)

(which-key-mode +1)

;; which-key-posframe
;; dependencies: which-key posframe
(require 'which-key-posframe)
(setq which-key-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))
(which-key-posframe-mode)

(provide 'init-which-key)
;;; init-which-key.el ends here
