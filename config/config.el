;;; config.el --- config file -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; general
(require 'general)

(require 'init-default)
(require 'init-func)
(require 'init-evil)
(require 'init-utils)
(require 'init-ui)
(require 'init-which-key)
(require 'init-pyim)
(require 'init-ivy)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-flycheck)
(require 'init-org)
(require 'init-project)
(require 'init-git)
(require 'init-lsp)
(require 'init-auto-save)
(require 'init-keyfreq)
(require 'init-keybinding)

(require 'init-lsp)
(require 'init-emacs-lisp)
(require 'init-graphviz-dot)
(require 'init-python)
(require 'init-cc)
(require 'init-latex)
(require 'init-rust)
(require 'init-R)
(require 'init-lean)
(require 'init-racket)

(when zenith/wsl-system
  (require 'init-wsl))

(provide 'config)
;;; config.el ends here
