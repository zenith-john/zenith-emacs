;;; init-auto-save.el --- auto-save configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(require 'auto-save)

(setq auto-save-idle 2
      auto-save-silent t
      auto-save-delete-trailing-whitespace t)

(auto-save-enable)

(provide 'init-auto-save)
;;; init-auto-save.el ends here
