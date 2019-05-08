;;; config.el -*- lexical-binding: t; -*-
;;; Code:

(require 'general)
(general-evil-setup)

(eval-when-compile (require 'use-package))

(require 'init-keyfreq)
(require 'init-org)
(require 'init-pyim)
(require 'init-english)
(require 'init-awesome-tab)
(require 'init-auto-save)
(require 'init-avy)
(require 'init-ace-window)
(require 'init-which-key)
(require 'init-lsp)
(require 'init-company)
(require 'init-ivy)
(require 'init-ui)
(require 'init-git)
(require 'init-pdf)

(require 'init-graphviz-dot)
(require 'init-python)
(require 'init-cc)
(require 'init-latex)

(provide 'config)
;;; config.el ends here
