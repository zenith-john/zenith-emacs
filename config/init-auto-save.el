;;; init-auto-save.el --- auto-save configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; The code is adjusted from https://github.com/manateelazycat/auto-save. The
;; problem of the original code is that it calls buffer-modified-p which makes
;; ws-butler unhappy.
(setq auto-save-idle 2)

(defun zenith/auto-save-buffers ()
  (interactive)
  (when (and
         (not (minibufferp))
         (buffer-file-name)
         (or (not (boundp 'yas--active-snippets))
             (not yas--active-snippets))
         (or (not (boundp 'company-candidates))
             (not company-candidates)))
    (with-temp-message ""
      (let ((inhibit-message t))
        (evil-write-all nil)))))

(defun zenith/auto-save-enable ()
  (interactive)
  (run-with-idle-timer auto-save-idle t #'zenith/auto-save-buffers))

(zenith/auto-save-enable)

(provide 'init-auto-save)
;;; init-auto-save.el ends here
