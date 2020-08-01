;;; init-evil.el --- evil configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; evil
;; dependencies: undo-tree goto-chg
(setq evil-want-integration t  ; This is optional since it's already set to t by default.
      evil-want-keybinding nil ; not loading keybindings
      evil-disable-insert-state-bindings t ; Use emacs's binding in insert state
      evil-want-C-d-scroll nil ; Use emacs's C-d
      evil-want-C-u-scroll nil ; Use emacs's C-u
      evil-want-C-i-jump t     ; Use vim's C-i
      evil-want-fine-undo t    ; Don not aggregate changes when exiting insert state
      evil-want-C-w-delete t   ; Use emacs's C-w
      evil-toggle-key ""       ; C-z not entering emacs state
      evil-respect-visual-line-mode t ; integration with visual-line-mode
      evil-move-beyond-eol t          ; emacs-like cursor movement
      evil-ex-substitute-global t     ; substitute global by default
      )

(require 'evil)

(evil-mode 1)

(evil-define-command evil-swiper (&optional search)
  "Invoke `swiper' with SEARCH"
  (interactive "<a>")
  (swiper search))
(evil-ex-define-cmd "sw[iper]" #'evil-swiper)

(evil-define-command evil-rg (&optional search)
  "Invoke `my-rg-project' with SEARCH"
  (interactive "<a>")
  (if search
      (rg-my-project search)
    (rg-dwim-project-dir)))
(evil-ex-define-cmd "rg" #'evil-rg)

(evil-define-command evil-external-open (&optional path)
  "open file with external program"
  (interactive "<f>")
  (zenith/open-by-external-program (or path
                                       (if (eq major-mode 'dired-mode)
                                           (dired-get-file-for-visit)
                                         (buffer-file-name (current-buffer))))))
(evil-ex-define-cmd "eo[pen]"    #'evil-external-open)

(evil-ex-define-cmd "git"        #'magit-status)
(evil-ex-define-cmd "gs[tage]"   #'magit-stage)
(evil-ex-define-cmd "gu[nstage]" #'magit-unstage)
(evil-ex-define-cmd "gb[lame]"   #'magit-blame)
(evil-ex-define-cmd "gp[rev]"    #'git-gutter:previous-hunk)
(evil-ex-define-cmd "gn[ext]"    #'git-gutter:next-hunk)
(evil-ex-define-cmd "gr[evert]"  #'git-gutter:revert-hunk)

(evil-ex-define-cmd "bm[ark]"    #'bookmark-set)
(evil-ex-define-cmd "bj[ump]"    #'bookmark-jump)
(evil-ex-define-cmd "bl[ist]"    #'bookmark-bmenu-list)

(evil-ex-define-cmd "bb"         #'ibuffer)

(evil-define-command evil-dired (&optional path)
  "open directory in dired"
  (interactive "<f>")
  (if path
      (dired path)
    (dired ".")))
(evil-ex-define-cmd "dir"        #'evil-dired)

(evil-define-command evil-fd-dired (&optional search)
  "Search file matches search"
  (interactive "<a>")
  (fd-dired "." search))
(evil-ex-define-cmd "fd"          #'evil-fd-dired)

;; evil-anzu
;; dependencies: evil anzu
(require 'evil-anzu)
(global-anzu-mode 1)

;; evil-pinyin
(require 'evil-pinyin)
(evil-select-search-module 'evil-search-module 'evil-search)
(global-evil-pinyin-mode 1)

;; evil-collection
;; dependencies: evil
(require 'evil-collection)
(setq evil-collection-company-use-tng nil)

(defun zenith/prefix-translations (_mode mode-keymaps &rest _rest)
  (evil-collection-swap-key 'normal mode-keymaps ";" ":"))

(add-hook 'evil-collection-setup-hook 'zenith/prefix-translations)

(evil-collection-init)

;; evil-surround
;; dependencies: evil
(require 'evil-surround)
(global-evil-surround-mode 1)

;; evil-nerd-commenter
;; dependencies: evil
(zenith/autoload
 '(evilnc-comment-operator
   evilnc-comment-or-uncomment-lines
   evilnc-comment-or-uncomment-paragraphs
   evilnc-comment-or-uncomment-to-the-line
   evilnc-copy-and-comment-lines
   evilnc-copy-and-comment-operator
   evilnc-copy-to-line)
 "evil-nerd-commenter")

;; evil-matchit
;; dependencies: evil
(require 'evil-matchit)
(setq evilmi-may-jump-by-percentage nil)
(global-evil-matchit-mode 1)

(provide 'init-evil)
;;; init-evil.el ends here
