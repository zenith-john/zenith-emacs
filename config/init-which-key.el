;;; init-which-key.el --- which-key configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(use-package which-key
  :defer 1
  :after-call pre-command-hook
  :init
  (setq which-key-sort-order #'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10)
  :config
  ;; general improvements to which-key readability
  (which-key-setup-side-window-bottom)

  (which-key-mode +1))

(provide 'init-which-key)
;;; init-which-key.el ends here
