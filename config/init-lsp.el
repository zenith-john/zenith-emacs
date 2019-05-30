;;; init-lsp.el --- lsp-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
;;; From https://github.com/hlissner/doom-emacs lsp module
(setq lsp-session-file (concat zenith-emacs-root-dir "local/lsp-session")
      lsp-auto-guess-root t
      lsp-keep-workspace-alive nil)

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map
    [remap +lookup/documentation] #'lsp-describe-thing-at-point)

  ;; Don't prompt to restart LSP servers while quitting Emacs
  (add-hook 'kill-emacs-hook (lambda ()(setq lsp-restart 'ignore))))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-use-webkit t
        lsp-ui-doc-header nil
        lsp-ui-doc-max-width 50
        lsp-ui-doc-max-height 30
        lsp-ui-doc-position 'at-point
        lsp-prefer-flymake nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-enable nil
        lsp-ui-doc-border (face-background 'default)
        lsp-ui-doc-webkit-client-path (concat "file://" zenith-emacs-extension-dir "lsp-ui-theme/lsp-ui-doc.html"))

  (setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 0)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t)))


  (general-define-key :keymaps lsp-ui-mode-map
    [remap xref-find-definitions] #'lsp-ui-peek-find-definitions
    [remap xref-find-references]  #'lsp-ui-peek-find-references
    ;; `set-lookup-handlers!' won't work for lsp-ui-peek commands, because they
    ;; don't switch buffers
    [remap +lookup/definition] #'lsp-ui-peek-find-definitions
    [remap +lookup/references] #'lsp-ui-peek-find-references)

  ;; Solve the conflict of awesome-tab-mode and lsp-ui-doc
  (defun lsp-ui-doc--mv-at-point-advice (args)
    (when (bound-and-true-p awesome-tab-mode)
      (setcar (nthcdr 3 args) (+ (nth 3 args) awesome-tab-height)))
    args)
  (advice-add 'lsp-ui-doc--mv-at-point :filter-args 'lsp-ui-doc--mv-at-point-advice))


(use-package company-lsp
  :after lsp-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
