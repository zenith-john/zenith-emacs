;;; init-ivy.el --- ivy-configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; Package
;;
;; (use-package ivy
;;   :defer 1
;;   :after-call pre-command-hook
;;   :config
;;   (setq ivy-height 15
;;         ivy-wrap t
;;         ivy-fixed-height-minibuffer t
;;         projectile-completion-system 'ivy
;;         ;; Don't use ^ as initial input
;;         ivy-initial-inputs-alist nil
;;         ;; highlight til EOL
;;         ivy-format-function #'ivy-format-function-line
;;         ;; enable magic slash on non-match
;;         ivy-magic-slash-non-match-action t
;;         ;; don't show recent files in switch-buffer
;;         ivy-use-virtual-buffers nil
;;         ;; ...but if that ever changes, show their full path
;;         ivy-virtual-abbreviate 'full
;;         ;; don't quit minibuffer on delete-error
;;         ivy-on-del-error-function nil
;;         ;; enable ability to select prompt (alternative to `ivy-immediate-done')
;;         ivy-use-selectable-prompt t)

;;   (with-eval-after-load 'yasnippet
;;     (add-to-list 'yas-prompt-functions #'+ivy-yas-prompt nil #'eq))

;;   (general-def ivy-mode-map
;;     [remap switch-to-buffer]              #'ivy-switch-buffer
;;     [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window
;;     [remap imenu-anywhere]                #'ivy-imenu-anywhere)

;;   (ivy-mode +1)

;;   (use-package ivy-hydra
;;     :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body)
;;     :init
;;     (general-def ivy-minibuffer-map
;;       "C-o" #'ivy-dispatching-done-hydra
;;       "M-o" #'hydra-ivy/body)
;;     :config
;;     ;; ivy-hydra rebinds this, so we have to do so again
;;     (define-key ivy-minibuffer-map (kbd "M-o") #'hydra-ivy/body)))

(after! ivy
  (setq ivy-magic-slash-non-match-action t))

(provide 'init-ivy)
;;; init-ivy.el ends here
