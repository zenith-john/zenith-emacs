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

(use-package company-lsp
  :after lsp-mode)

(provide 'init-lsp)
;;; init-lsp.el ends here
