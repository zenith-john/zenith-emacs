;;; init-awesome-tab.el --- awesome-tab configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(use-package awesome-tab
  :commands (awesome-tab-build-ivy-source awesome-tab-select-visible-tab awesome-tab-mode)
  :init
  (setq awesometab-hide-tabs-hooks
        '(magit-status-mode-hook magit-popup-mode-hook reb-mode-hook helpful-mode-hook)
        awesome-tab-style 'chamfer
        awesome-tab-label-fixed-length 14)

  (awesome-tab-mode t)
  (general-nvmap
   :prefix doom-leader-key
    "TAB" #'awesome-tab-build-ivy-source)
  (dotimes (i 10)
    (general-define-key (concat "M-" (int-to-string i)) #'awesome-tab-select-visible-tab)))

(provide 'init-awesome-tab)
;;; init-awesome-tab.el ends here
