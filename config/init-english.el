;;; init-english.el --- Emacs for english learning -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(use-package fd-dired
  :commands (fd-dired)
  :after (evil)
  :if (executable-find "fd")
  :init
  (evil-define-command +evil:fd (query)
    "Ex interface for fd-dired"
    (interactive "<a>")
    (fd-dired (file-name-directory (buffer-file-name)) query))
  (evil-ex-define-cmd "fd" #'+evil:fd))

(use-package sdcv
  :commands (sdcv-search-input+ sdcv-search-pointer+)
  :init
  (map! :ni "C-;" #'sdcv-search-pointer+)
  :config
  (setq sdcv-say-word-p t)

  (setq sdcv-dictionary-data-dir "/usr/share/stardict/dic/")

  (setq sdcv-dictionary-simple-list '("简明英汉字典增强版")))

(use-package company-english-helper
  :commands (toggle-company-english-helper))

(use-package insert-translated-name
  :commands (insert-translated-name-insert)
  :init
  (map! :i "C-'" #'insert-translated-name-insert)
  (defun +zenith/advice-insert-translated-name-active (style)
    (interactive "P")
    (add-hook 'after-change-functions 'insert-translated-name-monitor-after-change t t))
  (advice-add! 'insert-translated-name-active :before #'+zenith/advice-insert-translated-name-active))

(provide 'init-english)
;;; init-english.el ends here
