;;; config.el --- config file -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; general
(require 'general)
(eval-when-compile (require 'use-package))

(require 'init-default)
(require 'init-func)
(require 'init-undo-tree)
(require 'init-evil)
(require 'init-sp)
(require 'init-helpful)
(require 'init-project)
(require 'init-keyfreq)
(require 'init-org)
(require 'init-pyim)
(require 'init-eaf)
(require 'init-auto-save)
(require 'init-avy)
(require 'init-ace-window)
(require 'init-which-key)
(require 'init-lsp)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-ivy)
(require 'init-snails)
(require 'init-ui)
(require 'init-git)
(require 'init-keybinding)
(require 'init-flycheck)

(require 'init-emacs-lisp)
(require 'init-graphviz-dot)
(require 'init-python)
(require 'init-cc)
(require 'init-latex)

(provide 'config)
;;; config.el ends here
