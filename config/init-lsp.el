;;; init-lsp.el --- lsp-mode configuration -*- lexical-binding: t; -*-

(eval-after-load 'lsp-ui
  '(progn
     (defun lsp-ui-doc--mv-at-point-advice (args)
       (when (bound-and-true-p awesome-tab-mode)
         (setcar (nthcdr 3 args) (+ (nth 3 args) awesome-tab-height)))
         args)
     (advice-add 'lsp-ui-doc--mv-at-point :filter-args 'lsp-ui-doc--mv-at-point-advice)))

(provide 'init-lsp)
;;; init-lsp.el ends here
