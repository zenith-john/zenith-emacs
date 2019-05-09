;;; init-flycheck.el --- flycheck configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(use-package flycheck
  :hook ((python-mode . flycheck-mode)
         (c++-mode . flycheck-mode)
         (c-mode . flycheck-mode)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
