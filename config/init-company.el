;;; init-company.el --- completion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; Reconfigure company
(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 2)
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  (map! :i "C-x C-j" #'company-complete-common))

(provide 'init-company)
;;; init-company.el ends here
