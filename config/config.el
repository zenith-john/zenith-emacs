;;; config.el --- config file -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; general
(require 'general)

(require 'init-default)
(require 'init-func)
(require 'init-undo-tree)
(require 'init-ui)
(require 'init-evil)
(require 'init-sp)
(require 'init-helpful)
(require 'init-which-key)
(require 'init-pyim)
(require 'init-eaf)
(require 'init-ivy)
(require 'init-flycheck)

(run-with-idle-timer 1 nil
                     (lambda ()
                       (require 'init-auto-save)
                       (require 'init-company)
                       (require 'init-yasnippet)
                       (require 'init-lsp)
                       (require 'init-project)
                       (require 'init-snails)
                       (require 'init-keyfreq)
                       (require 'init-avy)
                       (require 'init-ace-window)
                       (require 'init-git)
                       (require 'init-org)))

(require 'init-keybinding)

(require 'init-emacs-lisp)
(require 'init-graphviz-dot)
(require 'init-python)
(require 'init-cc)
(require 'init-latex)

(provide 'config)
;;; config.el ends here
