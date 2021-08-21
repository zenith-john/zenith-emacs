;;; init-func.el --- define useful functions -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(defun +clear-image-cache ()
  "Remove image cache to redisplay the image."
  (interactive)
  (clear-image-cache))

(defun zenith/autoload (command-list file &optional non-interactive)
  "Autoload multiple function of the file at once"
  (dolist (command command-list)
    (autoload command file nil (not non-interactive))))

(defun zenith/add-hook (hook-list func)
  "Add one function to multiple hooks at once"
  (dolist (hook hook-list)
    (add-hook hook func)))

(defun zenith/delay-load (func)
  "Delay the evaluation of the function"
  (run-with-idle-timer 2 nil
                       (lambda ()
                         (zenith/temp-no-gc)
                         (funcall func)
                         (zenith/restore-gc))))

(defun zenith/is-space (char)
  "Check a char is whether a space character."
  (string-match-p "[\f\n\t\r ]" (char-to-string char)))

(defun zenith/is-char (char)
  (string-match-p "[[:alpha:]]" (char-to-string char)))

(defun zenith/is-ket (char)
  (string-match-p "[\]\}\)$]" (char-to-string char)))

(defun zenith/is-bra (char)
  (string-match-p "[\[\{\($]" (char-to-string char)))

(defun zenith/get-bare-file-name (&optional buffer)
  (let ((file (buffer-file-name buffer)))
    (file-name-sans-extension (file-name-base file))))

(defun zenith/open-by-external-program (path)
  "Open file in external program"
  (let ((display-buffer-alist '(("*Async Shell Command*" . (display-buffer-no-window))))
        (process-connection-type nil))
    (async-shell-command (format "nohup xdg-open \"%s\" >/dev/null 2>&1"
                                 (file-relative-name path default-directory)))))

;; Delete word in a more user friendly way
(defun zenith/aggressive-delete-space ()
  "Remove all the space until non-space character."
  (interactive)
  (let ((end (point))
        (begin (save-excursion
                 (re-search-backward "[^ \t\n\r]" nil t))))
    (delete-region (+ 1 begin) end)))

(defun zenith/delete-word-or-space ()
  "Remove all the space until non-space character if the char at
point and before are all space characters and delete word
otherwise."
  (interactive)
  (if (and (zenith/is-space (char-before))
           (zenith/is-space (char-before (- (point) 1))))
      (zenith/aggressive-delete-space)
    (backward-kill-word 1)))

(defun zenith/fill-and-indent-region ()
  "Fill paragraph and indent region at once"
  (interactive)
  (unless visual-line-mode
    (call-interactively 'fill-paragraph)))

;; function to unfill
(defun unfill-paragraph ()
  "Do the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (call-interactively 'fill-paragraph)))

(defun newline-dwim ()
  (interactive)
  (let ((break-open-pair (or (and (looking-back "{") (looking-at "}"))
                             (and (looking-back ">") (looking-at "<"))
                             (and (looking-back "(") (looking-at ")"))
                             (and (looking-back "\\[") (looking-at "\\]")))))
    (newline)
    (when break-open-pair
      (save-excursion
        (newline)
        (indent-for-tab-command)))
    (indent-for-tab-command)))

;; To remove gc limit temporarily
(defun zenith/temp-no-gc ()
  (setq gc-cons-threshold most-positive-fixnum))

;; Restore gc
(defun zenith/restore-gc ()
  (setq gc-cons-threshold zenith/low-gc-cons-threshold))

(provide 'init-func)
;;; init-func.el ends here
