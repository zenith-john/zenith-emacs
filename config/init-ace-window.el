;;; init-ace-window.el --- ace-window configuration -*- lexical-binding: t; -*-

(use-package ace-window
  :commands (ace-window)
  :init
  (general-define-key "M-'" 'ace-window))

(provide 'init-ace-window)
;;; init-ace-window.el ends here
