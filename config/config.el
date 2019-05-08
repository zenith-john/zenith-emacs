;;; config.el -*- lexical-binding: t; -*-
;;; Code:

(require 'general)
(general-evil-setup)

;; Set the font
(setq doom-font (font-spec :family "Iosevka Term SS09" :size 16 :weight 'semi-bold))
(setq doom-unicode-font (font-spec :family "Sarasa Term SC" :weight 'bold))

;; Change leader key to ,
(setq doom-leader-key ","
      doom-leader-alt-key "M-,"
      doom-localleader-key ", m"
      doom-localleader-alt-key "M-, m")

(require 'doom-autoload)
(require 'init-keyfreq)
(require 'init-latex)
(require 'init-org)
(require 'init-pyim)
(require 'init-english)
(require 'init-awesome-tab)
(require 'init-auto-save)
(require 'init-avy)
(require 'init-ace-window)
;;(require 'init-which-key)
(require 'init-lsp)
(require 'init-company)
(require 'init-ivy)
;;(require 'init-ui)
(require 'init-git)
;;(require 'init-evil)

(require 'init-graphviz-dot)
(require 'init-python)
(require 'init-tex)
(require 'init-cc)

;;; config.el ends here
