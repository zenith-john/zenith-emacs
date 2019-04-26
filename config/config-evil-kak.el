;;; config-evil-kak.el --- Evil mode like kakoune

;;; Commentary:
;;; Make evil-mode a little like kakoune for my personal use

;;; Code:
;;; TODO: Very Buggy cannot be used now
(defmacro evil-kak-new-region-wrap (fun)
  "Define evil-kak function to mark new region.
The function is based on evil function `FUN'."
  (let ((fun-name (format "evil-kak-new-region-%s" fun)))
    `(defun ,(intern fun-name) (&rest args)
       ,(format "The function start a new selection.\n
Based on %s , and has same `ARGS\'" fun)
       ,(interactive-form fun)
       (let ((begin (point))
             (flag 0))
         (when (evil-visual-state-p)
           (evil-exit-visual-state)
           (setq flag 1))
         (apply ',fun args)
         (let ((ptr (point)))
           (if (> ptr begin)
               (evil-visual-select begin ptr)
             (evil-visual-select ptr begin)))))))

(defmacro evil-kak-extend-wrap (fun)
  "Define evil-kak function to extend the selection region.
The function is based on evil function `FUN'"
  (let ((fun-name (format "evil-kak-extend-%s" fun)))
    `(defun ,(intern fun-name) (&rest args)
       ,(format "The function extends the selection region.\n
Based on %s , and has same `ARGS\'" fun)
       ,(interactive-form fun)
       (unless (evil-visual-state-p)
         (evil-visual-char))
       (apply ',fun args))))

(evil-kak-new-region-wrap evil-snipe-f)
(evil-kak-new-region-wrap evil-snipe-t)
(evil-kak-new-region-wrap evil-snipe-F)
(evil-kak-new-region-wrap evil-snipe-T)
(evil-kak-new-region-wrap evil-snipe-repeat)
(evil-kak-new-region-wrap evil-forward-word-end)
(evil-kak-new-region-wrap evil-backward-word-begin)
(evil-kak-new-region-wrap evil-forward-WORD-end)
(evil-kak-new-region-wrap evil-backward-WORD-begin)

(evil-kak-extend-wrap evil-snipe-f)
(evil-kak-extend-wrap evil-snipe-t)
(evil-kak-extend-wrap evil-snipe-F)
(evil-kak-extend-wrap evil-snipe-T)
(evil-kak-extend-wrap evil-snipe-repeat)
(evil-kak-extend-wrap evil-forward-word-end)
(evil-kak-extend-wrap evil-backward-word-begin)
(evil-kak-extend-wrap evil-forward-WORD-end)

(defun evil-kak-delete ()
  "Delete according to whether there is active region."
  (interactive)
  (if mark-active
      (call-interactively #'kill-region)
    (call-interactively #'delete-char)))

(defun evil-kak-change ()
  "Change according to whether there is active region."
  (interactive)
  (evil-kak-delete)
  (evil-insert))

(defun evil-kak-deactive ()
  "Deactive the selection region."
  (interactive)
  (evil-exit-visual-state))

(map! :nv "d" #'evil-kak-delete
      :nv "c" #'evil-kak-change
      :nv "x" #'evil-kak-deactive
      :nv "b" #'evil-kak-new-region-evil-backward-word-begin
      :nv "B" #'evil-kak-new-region-evil-backward-WORD-begin
      :nv "e" #'evil-kak-new-region-evil-forward-word-end
      :nv "E" #'evil-kak-new-region-evil-forward-WORD-end
      :nv "f" #'evil-kak-new-region-evil-snipe-f
      :nv "F" #'evil-kak-new-region-evil-snipe-F
      :nv "t" #'evil-kak-new-region-evil-snipe-t
      :nv "T" #'evil-kak-new-region-evil-snipe-T
      :v "M-b" #'evil-kak-extend-backward-word-begin
      :v "M-B" #'evil-kak-extend-backward-WORD-begin
      :v "M-e" #'evil-kak-extend-forward-word-end
      :v "M-E" #'evil-kak-extend-forward-WORD-end
      :v "M-f" #'evil-kak-extend-snipe-f
      :v "M-F" #'evil-kak-extend-snipe-F
      :v "M-t" #'evil-kak-extend-snipe-t
      :v "M-T" #'evil-kak-extend-snipe-T
      )

(provide 'config-evil-kak)
;;; config-evil-kak.el ends here
