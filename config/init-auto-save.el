;;; init-auto-save.el --- auto-save configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(require 'auto-save)

(setq auto-save-idle 2
      auto-save-silent t
      auto-save-delete-trailing-whitespace nil)

;; The following code makes auto-save.el work with ws-butler. The save-excursion
;; in auto-save.el automatically change the cursor position even if the virtual
;; space is reserved by ws-butler.
(defun zenith/auto-save-buffers ()
  (interactive)
  (auto-save-buffers)
  (when ws-butler-mode
    (ws-butler-after-save)))

(defun zenith/auto-save-enable ()
  (interactive)
  (run-with-idle-timer auto-save-idle t #'zenith/auto-save-buffers))

(zenith/auto-save-enable)

(provide 'init-auto-save)
;;; init-auto-save.el ends here
