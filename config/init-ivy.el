;;; init-ivy.el --- ivy-configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; Reconfigure ivy
(with-eval-after-load 'ivy
  (setq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected))


(provide 'init-ivy)
;;; init-ivy.el ends here
