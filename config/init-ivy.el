;;; init-ivy.el --- ivy-configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; Package
;; swiper
(require 'ivy)
(require 'counsel)

(setq ivy-height 20
      ivy-wrap t
      ivy-extra-directories nil
      ivy-fixed-height-minibuffer t
      projectile-completion-system 'ivy
      ;; Don't use ^ as initial input
      ivy-initial-inputs-alist nil
      ;; enable magic slash on non-match
      ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
      ;; don't show recent files in switch-buffer
      ivy-use-virtual-buffers nil
      ;; ...but if that ever changes, show their full path
      ivy-virtual-abbreviate 'full
      ;; don't quit minibuffer on delete-error
      ivy-on-del-error-function nil
      ;; enable ability to select prompt (alternative to `ivy-immediate-done')
      ivy-use-selectable-prompt t
      ;; highlight until EOL
      ivy-format-functions-alist
      '((t . ivy-format-function-line)))


;; Don't show annoying helpful buffer in buffer selection list
(add-to-list 'ivy-ignore-buffers "\*helpful")

(ivy-mode +1)

(when zenith/enable-posframe
  ;; ivy-posframe
  ;; dependencies: ivy posframe
  (require 'ivy-posframe)

  (defvar zenith/ivy-posframe-width 100
    "width of the ivy-posframe")

  (defun zenith/ivy-posframe-get-size ()
    "Set size of by the posframe"
    (list
     :min-height 3
     :height 23
     :width zenith/ivy-posframe-width
     :min-width zenith/ivy-posframe-width))

  (defun ivy-posframe-display-at-frame-center/zenith (str)
    (ivy-posframe--display str #'zenith/posframe-poshandler-frame-center))

  (defun zenith/posframe-poshandler-frame-center (info)
    "Put the posframe at frame center, while keep the top row fixed"
    (cons (/ (- (plist-get info :parent-frame-width)
                (+ (* (plist-get info :font-width) zenith/ivy-posframe-width) 2))
             2)
          (/ (- (plist-get info :parent-frame-height)
                (+ (* (plist-get info :font-height) ivy-posframe-height) 2))
             2)))

  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center/zenith))
        ivy-posframe-size-function #'zenith/ivy-posframe-get-size
        ivy-posframe-height 20)
  (ivy-posframe-mode)
  ;; TODO The problem is that you can not use pyim when ivy-posframe exists.
  ;; Therefore it can be annoying to have ivy-posframe open when some chinese
  ;; characters are going to be input
  )

;; all-the-icons-ivy
;; dependencies: ivy all-the-icons
(require 'all-the-icons-ivy)

;; Redefine `all-the-icons-ivy-file-transformer' and
;; `all-the-icons-ivy-buffer-transformer' to adjust it to work in terminal.
(defun all-the-icons-ivy-file-transformer (s)
  "Return a candidate string for filename S preceded by an icon."
  (if (window-system)
      (format (concat "%s" all-the-icons-spacer "%s")
              (propertize "\t" 'display (all-the-icons-ivy-icon-for-file s))
              s)
    s))

(defun all-the-icons-ivy-buffer-transformer (s)
  "Return a candidate string for buffer named S.
Assume that sometimes the buffer named S might not exists.
That can happen if `ivy-switch-buffer' does not find the buffer and it
falls back to `ivy-recentf' and the same transformer is used."
  (if (window-system)
      (let ((b (get-buffer s)))
        (if b
            (all-the-icons-ivy--buffer-transformer b s)
          (all-the-icons-ivy-file-transformer s)))
    s))

(all-the-icons-ivy-setup)
(with-eval-after-load 'counsel-projectile
  (let ((all-the-icons-ivy-file-commands '(counsel-projectile
                                           counsel-projectile-find-file
                                           counsel-projectile-find-dir))
        (all-the-icons-ivy-buffer-commands
         '(counsel-projectile-switch-to-buffer)))
    (all-the-icons-ivy-setup)))

(setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
      counsel-find-file-extern-extensions zenith/external-extensions
      counsel-describe-function-function #'helpful-callable
      counsel-describe-variable-function #'helpful-variable
      ;; Add smart-casing (-S) to default command arguments:
      counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
      counsel-ag-base-command "ag -S --nocolor --nogroup %s"
      counsel-pt-base-command "pt -S --nocolor --nogroup -e %s")

;; Configure `counsel-find-file'
(ivy-add-actions
 'counsel-find-file
 '(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("s" counsel-find-file-as-root "open as root")
   ("m" counsel-find-file-mkdir-action "mkdir")
   ("e" zenith/open-by-external-program "external program")
   ("f" find-file-other-window "other window")
   ("F" find-file-other-frame "other frame")
   ("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory)))) "insert relative path")
   ("P" (lambda (path) (with-ivy-window (insert path))) "insert absolute path")
   ("l" (lambda (path) "Insert org-link with relative path"
          (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory))))) "insert org-link (rel. path)")
   ("L" (lambda (path) "Insert org-link with absolute path"
          (with-ivy-window (insert (format "[[%s]]" path)))) "insert org-link (abs. path)")))

(defun zenith/ivy-fuzzy-ignore-order-helper (str)
  (cons (ivy--regex-fuzzy str) t))

(defun zenith/ivy-fuzzy-ignore-order (str)
  (let ((str-list (split-string str)))
    (if str-list
        (mapcar 'zenith/ivy-fuzzy-ignore-order-helper str-list)
      "")))

(if zenith/enable-pyim
    (progn
      ;; https://github.com/tumashu/emacs-helper/commit/1932a9e8a64f08bb9603cf244df41f6c0bbc3dac
      ;; Search chinese with pinyin
      (defun zenith/ivy-cregexp-helper (str)
        (cons (pyim-cregexp-build str) t))

      (defun zenith/ivy-cregexp-ignore-order (str)
        (let ((str-list (split-string str)))
          (if str-list
              (mapcar 'zenith/ivy-cregexp-helper str-list)
            ""))))
  (defalias 'zenith/ivy-cregexp-ignore-order 'zenith/ivy-fuzzy-ignore-order))

(defun zenith/toggle-char (char)
  (cond
   ((and (<= char ?Z) (>= char ?A)) (+ char 32))
   ((and (<= char ?z) (>= char ?a)) (- char 32))
   (t char)))

(defun zenith/toggle-case (str)
  (let (ret)
    (dolist (char (string-to-list str))
      (setq ret (concat ret (char-to-string (zenith/toggle-char char)))))
    ret))

(defun zenith/string< (x y)
  (string< (zenith/toggle-case (if (consp x) (car x) x))
           (zenith/toggle-case (if (consp y) (car y) y))))

(defun zenith/sort (_name cands)
  (cl-sort (copy-sequence cands) #'zenith/string<))

(add-to-list 'ivy-sort-matches-functions-alist '(LaTeX-environment . zenith/sort))
(setq ivy-re-builders-alist '((counsel-company . zenith/ivy-fuzzy-ignore-order)
                              (t . zenith/ivy-cregexp-ignore-order)))

;; amx
;; depednecies: s
(require 'amx)
(setq amx-save-file (expand-file-name "amx-items" zenith-emacs-local-dir))
(amx-mode)

(provide 'init-ivy)
;;; init-ivy.el ends here
