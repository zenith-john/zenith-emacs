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
(require 'init-which-key)
(require 'init-pyim)
(require 'init-eaf)
(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-org)
(require 'init-project)
(require 'init-snails)
(require 'init-git)

(run-with-idle-timer 1 nil
                     (lambda ()
                       (require 'init-auto-save)
                       (require 'init-keyfreq)
                       (require 'init-ace-window)
                       (require 'init-helpful)
                       (require 'init-avy)
                       (require 'init-lsp)
                       (require 'org)))

(require 'init-keybinding)

(require 'init-emacs-lisp)
(require 'init-graphviz-dot)
(require 'init-python)
(require 'init-cc)
(require 'init-latex)

(provide 'config)
;;; config.el ends here
