;;; init-keyfreq.el --- keyfreq-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(use-package keyfreq
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
                                    backward-delete-char-untabify
                                    exit-minibuffer
                                    exit-minibuffer
                                    ace-window
                                    awesome-tab-select-visible-tab
                                    company-complete-selection
                                    newline-and-indentj
                                    )))

(provide 'init-keyfreq)
;;; init-keyfreq.el ends here
