;;; init-yasnippet.el --- yasnippet configuration -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:
(use-package yasnippet
  :init
  (setq yas-snippet-dirs `(,(concat zenith-emacs-extension-dir "snippets/")))
  :config
  (yas-global-mode +1)
  (with-eval-after-load 'company
    (general-def company-active-map [tab] yas-maybe-expand)))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
