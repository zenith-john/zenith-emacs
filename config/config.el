;;; config.el -*- lexical-binding: t; -*-
(def-package! keyfreq
  :commands (keyfreq-mode keyfreq-show keyfreq-reset)
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands '(org-self-insert-command
                                    self-insert-command
                                    next-line
                                    previous-line
                                    right-char
                                    left-char
                                    cua-scroll-down
                                    company-ignore
                                    org-delete-backward-char
                                    python-indent-dedent-line
                                    python-indent-dedent-line-backspace
                                    delete-backward-char
                                    org-return
                                    ivy-next-line
                                    ivy-backward-delete-char
                                    company-select-next-or-abort
                                    company-select-previous-or-abort
                                    end-of-line
                                    magit-next-line
                                    mwheel-scroll
                                    isearch-printing-char
                                    newline
                                    mouse-drag-region
                                    org-cycle
                                    ivy-previous-line
                                    org-meta-return
                                    mouse-set-point
                                    kill-line
                                    find-file
                                    org-agenda-next-line
                                    ivy-done
                                    minibuffer-keyboard-quit
                                    magit-previous-line
                                    beginning-of-line
                                    indent-for-tab-command
                                    evil-previous-line
                                    evil-next-line
                                    backward-delete-char-untabify
                                    evil-forward-char
                                    exit-minibuffer
                                    evil-backward-char
                                    exit-minibuffer
                                    evil-ex
                                    evil-normal-state
                                    )))
(use-package org-noter
  :commands (org-noter)
  :after pdf-tools
  :config
  ;; Overriding the function due to make visual-line mode no effect.
  (defun org-noter--set-notes-scroll (window &rest ignored)
    nil)
  (setq org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil
        org-noter-hide-other nil)

  (map! :map pdf-view-mode-map :gn "C-i" #'org-noter-insert-note-toggle-no-questions)
  (map! :map pdf-view-mode-map :gn "q" #'org-noter-kill-session)
  (map! :map pdf-view-mode-map :gn "i" #'org-noter-insert-note))

(use-package fd-dired
  :commands (fd-dired)
  :after (evil)
  :init
  (when (executable-find "fd")
      (evil-define-command +evil:fd (query)
        "Ex interface for fd-dired"
        (interactive "<a>")
        (fd-dired (file-name-directory (buffer-file-name)) query))
      (evil-ex-define-cmd "fd" #'+evil:fd)))

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

(use-package auto-save
  :init
  (require 'auto-save)

  (setq auto-save-idle 2
        auto-save-silent t
        auto-save-delete-trailing-whitespace t)

  (auto-save-enable))

(use-package awesome-tab
  :commands (awesome-tab-build-ivy-source awesome-tab-select-visible-tab awesome-tab-mode)
  :init
  (setq awesometab-hide-tabs-hooks
        '(magit-status-mode-hook magit-popup-mode-hook reb-mode-hook helpful-mode-hook))
  (setq awesome-tab-style 'chamfer
        awesome-tab-label-fixed-length 14)
  (awesome-tab-mode t)
  (map! :leader
        :nv
        "TAB" #'awesome-tab-build-ivy-source)
  (dotimes (i 10)
    (map! :nvi (concat "M-" (int-to-string i)) #'awesome-tab-select-visible-tab)))

;;; config.el ends here
