;;; init-pyim.el --- pyim configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

(when zenith/enable-pyim
  ;; pyim
  (zenith/autoload '(pyim-cregexp-build pyim-string-match-p pyim-char-before-to-string) "pyim")
  ;; (require 'pyim)

  (with-eval-after-load 'pyim
    (add-to-list 'pyim-punctuation-dict '("\\" "、")))

  ;; emacs-rime: use emacs-rime to replace pyim because it is simpler faster but
  ;; pyim provides some useful functions, so I won't throw it away, instead I make
  ;; some minor changes to make it usable to emacs-rime
  (require 'rime)

  (setq
   rime-user-data-dir (concat zenith-emacs-local-dir "pyim/rime")
   default-input-method "rime"
   rime-show-candidate (if zenith/enable-posframe
                           'posframe
                         'minibuffer))

  (defun rime-predicate-line-start ()
    "Check whether the point is beginning of the line"
    (save-excursion
      (let ((pt (point)))
        (beginning-of-line)
        (equal (point) pt))))

  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-ascii-char-p
          rime-predicate-space-after-ascii-p
          rime-predicate-line-start
          rime-predicate-prog-in-code-p
          rime-predicate-ace-window-p
          rime-predicate-hydra-p
          rime-predicate-current-uppercase-letter-p
          rime-predicate-tex-math-or-command-p))

;;; support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-l)

  (defun zenith/rime-force-enable ()
    (interactive)
    (activate-input-method "rime")
    (rime-force-enable))

  ;;；Adapted from pyim
  (defun zenith/rime-convert-string-at-point ()
    "将光标前的用户输入的字符串转换为中文."
    (interactive)
    (activate-input-method "rime")
    (let* ((case-fold-search nil)
           (string (if mark-active
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (buffer-substring (point) (line-beginning-position))))
           code length)
      (cond
       ((string-match
         "[a-z]+ *$"
         string)
        (setq code
              ;; 一些编程语言使用单引号 ' 做为字符串的标记，这里需要特殊处理。
              (replace-regexp-in-string
               "^[-']" ""
               (match-string 0 string)))
        (setq length (length code))
        (setq code (replace-regexp-in-string " +" "" code))
        (when mark-active
          (delete-region
           (region-beginning) (region-end)))
        (when (and (not mark-active) (> length 0))
          (delete-char (- 0 length)))
        (rime-force-enable)
        (when (> length 0)
          (setq unread-command-events
                (append (listify-key-sequence code)
                        unread-command-events))))
       ((pyim-string-match-p "[[:punct:]：－]" (pyim-char-before-to-string 0))
        ;; 当光标前的一个字符是标点符号时，半角/全角切换。
        (call-interactively 'pyim-punctuation-translate-at-point))
       (t (message "Rime: rime-convert-string-at-point do noting."))))))

(provide 'init-pyim)
;;; init-pyim.el ends here
