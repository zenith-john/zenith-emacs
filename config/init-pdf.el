;;; init-pdf.el --- pdf support -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; pdf-tools
;; dependencies: tablist let-alist
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (when noninteractive
    (pdf-tools-install))
  (general-def pdf-view-mode-map
    "q" 'kill-this-buffer
    "k" 'pdf-view-scroll-down-or-previous-page
    "j" 'pdf-view-scroll-up-or-next-page)
  (setq-default pdf-view-display-size 'fit-page)
  ;; Turn on doom-modeline for pdf file
  (add-hook 'pdf-view-mode-hook '(lambda () (display-line-numbers-mode 0)))
  (with-eval-after-load 'doom-modeline
    (add-hook 'pdf-view-mode-hook #'doom-modeline-set-pdf-modeline)))

(provide 'init-pdf)
;;; init-pdf.el ends here



