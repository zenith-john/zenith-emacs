;;; init-yasnippet.el --- yasnippet configuration -*- lexical-binding: t -*-

;;; Commentary:
;;;

;;; Code:

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs `(,(concat zenith-emacs-extension-dir "snippets/")))

(yas-global-mode +1)
(with-eval-after-load 'company
  (general-def company-active-map [tab] yas-maybe-expand))
(general-def "C-." 'yas-insert-snippet)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
