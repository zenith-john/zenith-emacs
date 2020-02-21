;;; init-ace-window.el --- ace-window configuration -*- lexical-binding: t; -*-

;; ace-window
;; dependencies: avy
(use-package ace-window
  :commands (ace-window)
  :init
  (general-define-key "M-o" 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame
        aw-background t))

(provide 'init-ace-window)
;;; init-ace-window.el ends here
