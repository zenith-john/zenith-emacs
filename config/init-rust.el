;;; init-ui.el --- ui configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(setq rust-always-locate-project-on-open t)

(defun zenith/rust-flycheck-hook ()
  (flycheck-mode 1)
  (setq-local flycheck-checker 'rust-clippy))
(add-hook 'rust-mode-hook 'zenith/rust-flycheck-hook)

(provide 'init-rust)
;;; init-rust.el ends here
