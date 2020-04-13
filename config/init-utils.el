;;; init-utils.el ---  configure some useful packages -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode 1)

;; smartparens
(require 'smartparens-config)

(setq-default sp-autoskip-closing-pair t) ;; manually add closing pair work as I like

(zenith/add-hook '(comint-mode-hook prog-mode-hook LaTeX-mode-hook org-mode-hook) 'smartparens-mode)

;; format-all
(autoload 'format-all-buffer "format-all" nil t)

;; ibuffer-projectile
;; dependencies: projectile
(add-hook 'ibuffer-hook
          (lambda ()
            (require 'ibuffer-projectile)
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))
            (setq-default ibuffer-formats
                          '((mark modified read-only " "
                             (name 18 18 :left :elide)
                             " "
                             (size 9 -1 :right)
                             " "
                             (mode 16 16 :left :elide)
                             " "
                             project-relative-file)))))

;; ws-butler
(require 'ws-butler)
(ws-butler-global-mode 1)

;; emacs-wgrep
(autoload 'wgrep-change-to-wgrep-mode "wgrep")
(setq wgrep-auto-save-buffer t)

;; Mode load
;; markdown-mode
(dolist (elt '(("\\.md\\'" . markdown-mode)
               ("\\.markdown\\'" . markdown-mode)))
  (add-to-list 'auto-mode-alist elt))

(autoload 'markdown-mode "markdown-mode")

;; rg.el
;; depedencies: s transient wgrep
(zenith/autoload '(rg rg-project rg-dwim-project-dir) "rg")

;; ace-window
;; dependencies: avy
(autoload 'ace-window "ace-window")
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-scope 'frame
      aw-background t)

;; helpful
;; dependencies: dash dash-functional s f elisp-refs
;; elisp-refs depends on loop
(zenith/autoload '(helpful-callable helpful-variable helpful-key) "helpful")

;; avy
(zenith/autoload '(avy-goto-char avy-goto-char-in-line) "avy")
(setq avy-background t
      avy-all-windows nil)

(provide 'init-utils)
