;;; init-ui.el --- ui configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; emacs-application-framework
(autoload 'eaf-open "eaf" nil t)
(setq eaf-find-alternate-file-in-dired t)

(setq browse-url-browser-function 'eaf-open-browser)
(defalias 'browse-web #'eaf-open-browser)

(with-eval-after-load 'eaf
  (eaf-setq eaf-pdf-dark-mode "false"))

(provide 'init-eaf)
;;; init-eaf.el ends here
