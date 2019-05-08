;;; init-pdf.el --- pdf support -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (when noninteractive
    (pdf-tools-install))
  (general-nmap :keymaps 'pdf-view-mode-map "q" 'kill-this-buffer)
  (setq-default pdf-view-display-size 'fit-page)
  ;; Turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0))))

(provide 'init-pdf)
;;; init-pdf.el ends here



