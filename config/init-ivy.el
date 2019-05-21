;;; init-ivy.el --- ivy-configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; Package
;;
(use-package ivy
  :config
  (setq ivy-height 15
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line
        ;; enable magic slash on non-match
        ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function nil
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)

  (general-def ivy-mode-map
    [remap switch-to-buffer]              #'ivy-switch-buffer
    [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window
    [remap imenu-anywhere]                #'ivy-imenu-anywhere)
  
  (general-def ivy-minibuffer-map [escape] #'keyboard-escape-quit)

  ;; Don't show annoying helpful buffer in buffer selection list
  (add-to-list 'ivy-ignore-buffers "\*helpful")

  (ivy-mode +1)
  (use-package ivy-prescient
    :after ivy
    :config
    (setq ivy-prescient-retain-classic-highlighting t)
    (ivy-prescient-mode))

  (use-package ivy-hydra
    :commands (ivy-dispatching-done-hydra ivy--matcher-desc ivy-hydra/body)
    :init
    (general-def ivy-minibuffer-map
      "C-o" #'ivy-dispatching-done-hydra
      "M-o" #'hydra-ivy/body)
    :config
    ;; ivy-hydra rebinds this, so we have to do so again
    (define-key ivy-minibuffer-map (kbd "M-o") #'hydra-ivy/body)))

(use-package ivy-rich
  :after ivy
  :config
  ;; Remove built-in coloring of buffer list; we do our own
  (setq ivy-switch-buffer-faces-alist nil)
  (ivy-set-display-transformer 'internal-complete-buffer nil)

  ;; Allow these transformers to apply to more switch-buffer commands
  (let ((ivy-switch-buffer-transformer (plist-get ivy-rich-display-transformers-list 'ivy-switch-buffer)))
    (dolist (cmd '(counsel-projectile-switch-to-buffer))
      (setq ivy-rich-display-transformers-list
            (plist-put ivy-rich-display-transformers-list
                       cmd ivy-switch-buffer-transformer))))

  ;; Reload ivy which so changes to `ivy-rich-display-transformers-list' work
  (ivy-rich-mode +1))


(use-package all-the-icons-ivy
  :after ivy
  :config
  ;; `all-the-icons-ivy' is incompatible with ivy-rich's switch-buffer
  ;; modifications, so we disable them and merge them ourselves
  (setq all-the-icons-ivy-buffer-commands nil)

  (all-the-icons-ivy-setup)
  (with-eval-after-load 'counsel-projectile
    (let ((all-the-icons-ivy-file-commands '(counsel-projectile
                                             counsel-projectile-find-file
                                             counsel-projectile-find-dir)))
      (all-the-icons-ivy-setup))))


(use-package counsel
  :commands counsel-describe-face
  :init
  (general-def
    [remap apropos]                  #'counsel-apropos
    [remap bookmark-jump]            #'counsel-bookmark
    [remap describe-face]            #'counsel-faces
    [remap describe-function]        #'counsel-describe-function
    [remap describe-variable]        #'counsel-describe-variable
    [remap describe-bindings]        #'counsel-descbinds
    [remap set-variable]             #'counsel-set-variable
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap find-library]             #'counsel-find-library
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap imenu]                    #'counsel-imenu
    [remap recentf-open-files]       #'counsel-recentf
    [remap org-capture]              #'counsel-org-capture
    [remap swiper]                   #'counsel-grep-or-swiper
    [remap evil-ex-registers]        #'counsel-evil-registers
    [remap yank-pop]                 #'counsel-yank-pop)
  :config
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable
        ;; Add smart-casing (-S) to default command arguments:
        counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
        counsel-ag-base-command "ag -S --nocolor --nogroup %s"
        counsel-pt-base-command "pt -S --nocolor --nogroup -e %s")

  ;; Record in jumplist when opening files via counsel-{ag,rg,pt,git-grep}
  ;; (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)

  ;; Factories
  (defun +ivy-action-reloading (cmd)
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))

  (defun +ivy-action-given-file (cmd prompt)
    (lambda (source)
      (let* ((enable-recursive-minibuffers t)
             (target (read-file-name (format "%s %s to:" prompt source))))
        (funcall cmd source target 1))))

  ;; Configure `counsel-find-file'
  (ivy-add-actions
   'counsel-find-file
   `(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
     ("s" counsel-find-file-as-root "open as root")
     ("m" counsel-find-file-mkdir-action "mkdir")
     ("c" ,(+ivy-action-given-file #'copy-file "Copy file") "copy file")
     ("d" ,(+ivy-action-reloading #'+ivy-confirm-delete-file) "delete")
     ("r" (lambda (path) (rename-file path (read-string "New name: "))) "rename")
     ("R" ,(+ivy-action-reloading (+ivy-action-given-file #'rename-file "Move")) "move")
     ("f" find-file-other-window "other window")
     ("F" find-file-other-frame "other frame")
     ("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory)))) "insert relative path")
     ("P" (lambda (path) (with-ivy-window (insert path))) "insert absolute path")
     ("l" (lambda (path) "Insert org-link with relative path"
            (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory))))) "insert org-link (rel. path)")
     ("L" (lambda (path) "Insert org-link with absolute path"
            (with-ivy-window (insert (format "[[%s]]" path)))) "insert org-link (abs. path)")))

  (ivy-add-actions
   'counsel-ag ; also applies to `counsel-rg' & `counsel-pt'
   '(("O" +ivy-git-grep-other-window-action "open in other window"))))


(use-package counsel-projectile
  :commands (counsel-projectile-find-file
             counsel-projectile-find-dir
             counsel-projectile-switch-to-buffer
             counsel-projectile-grep
             counsel-projectile-ag
             counsel-projectile-switch-project)
  :init
  (general-def
    [remap projectile-find-dir]         #'counsel-projectile-find-dir
    [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
    [remap projectile-grep]             #'counsel-projectile-grep
    [remap projectile-ag]               #'counsel-projectile-ag
    [remap projectile-switch-project]   #'counsel-projectile-switch-project)
  :config
  ;; no highlighting visited files; slows down the filtering
  (ivy-set-display-transformer #'counsel-projectile-find-file nil))


(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(use-package flx
  :defer t  ; is loaded by ivy
  :init
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (counsel-grep . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (t . ivy--regex-fuzzy))
        ivy-initial-inputs-alist nil))

(provide 'init-ivy)
;;; init-ivy.el ends here
