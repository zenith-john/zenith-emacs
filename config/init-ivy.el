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

;; ivy-posframe
;; dependencies: ivy posframe
(require 'ivy-posframe)

(defun zenith/ivy-posframe-get-size ()
  "Set size of by the posframe"
  (list
   :height (max (+ 3 ivy-height) ivy-posframe-height)
   :width (round (* 0.5 (frame-width)))
   :min-height (max (+ 3 ivy-height) ivy-posframe-height)
   :min-width (max 80 (round (* 0.5 (frame-width))))))

(defun ivy-posframe--display (str &optional poshandler)
  "Show STR in ivy's posframe with POSHANDLER."
  (if (not (posframe-workable-p))
      (ivy-display-function-fallback str)
    (with-ivy-window
      (apply #'posframe-show
             ivy-posframe-buffer
             :font ivy-posframe-font
             :string str
             :position (point)
             :poshandler poshandler
             :lines-truncate t
             :background-color (face-attribute 'ivy-posframe :background nil t)
             :foreground-color (face-attribute 'ivy-posframe :foreground nil t)
             :internal-border-width ivy-posframe-border-width
             :internal-border-color (face-attribute 'ivy-posframe-border :background nil t)
             :override-parameters ivy-posframe-parameters
             (funcall ivy-posframe-size-function))
     (ivy-posframe--add-prompt 'ignore))))

(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
      ivy-posframe-size-function #'zenith/ivy-posframe-get-size
      ivy-posframe-height 20)
(ivy-posframe-mode)

;; all-the-icons-ivy
;; dependencies: ivy all-the-icons
(require 'all-the-icons-ivy)

(all-the-icons-ivy-setup)
(with-eval-after-load 'counsel-projectile
  (let ((all-the-icons-ivy-file-commands '(counsel-projectile
                                           counsel-projectile-find-file
                                           counsel-projectile-find-dir))
        (all-the-icons-ivy-buffer-commands
         '(counsel-projectile-switch-to-buffer)))
    (all-the-icons-ivy-setup)))

(setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
      counsel-describe-function-function #'helpful-callable
      counsel-describe-variable-function #'helpful-variable
      ;; Add smart-casing (-S) to default command arguments:
      counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
      counsel-ag-base-command "ag -S --nocolor --nogroup %s"
      counsel-pt-base-command "pt -S --nocolor --nogroup -e %s")

;; Record in jumplist when opening files via counsel-{ag,rg,pt,git-grep}
;; (add-hook 'counsel-grep-post-action-hook #'better-jumper-set-jump)

;; Factories
(defun +ivy-action-reloading (cmd)
  (lambda (x)
    (funcall cmd x)
    (ivy--reset-state ivy-last)))

(defun +ivy-action-given-file (cmd prompt)
  (lambda (source)
    (let* ((enable-recursive-minibuffers t)
           (target (read-file-name (format "%s %s to:" prompt source))))
      (funcall cmd source target 1))))

(defun zenith/open-by-external-program (path)
  "Open file in external program"
  (let ((display-buffer-alist '(("*Async Shell Command*" . (display-buffer-no-window)))))
    (async-shell-command (format "nohup xdg-open \"%s\" >/dev/null 2>&1"
                                 (file-relative-name path default-directory)))))

;; Configure `counsel-find-file'
(ivy-add-actions
 'counsel-find-file
 `(("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("s" counsel-find-file-as-root "open as root")
   ("m" counsel-find-file-mkdir-action "mkdir")
   ("c" ,(+ivy-action-given-file #'copy-file "Copy file") "copy file")
   ("d" ,(+ivy-action-reloading #'+ivy-confirm-delete-file) "delete")
   ("r" (lambda (path) (rename-file path (read-string "New name: "))) "rename")
   ("R" ,(+ivy-action-reloading (+ivy-action-given-file #'rename-file "Move")) "move")
   ("e" zenith/open-by-external-program "external program")
   ("f" find-file-other-window "other window")
   ("F" find-file-other-frame "other frame")
   ("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory)))) "insert relative path")
   ("P" (lambda (path) (with-ivy-window (insert path))) "insert absolute path")
   ("l" (lambda (path) "Insert org-link with relative path"
          (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory))))) "insert org-link (rel. path)")
   ("L" (lambda (path) "Insert org-link with absolute path"
          (with-ivy-window (insert (format "[[%s]]" path)))) "insert org-link (abs. path)")))

;; https://github.com/tumashu/emacs-helper/commit/1932a9e8a64f08bb9603cf244df41f6c0bbc3dac
;; Search chinese with pinyin
(defun zenith/ivy-cregexp-helper (str)
  (cons (pyim-cregexp-build str) t))

(defun zenith/ivy-cregexp-ignore-order (str)
  (let ((str-list (split-string str)))
    (if str-list
        (mapcar 'zenith/ivy-cregexp-helper str-list)
      "")))

(defun zenith/ivy-fuzzy-ignore-order-helper (str)
  (cons (ivy--regex-fuzzy str) t))

(defun zenith/ivy-fuzzy-ignore-order (str)
  (let ((str-list (split-string str)))
    (if str-list
        (mapcar 'zenith/ivy-fuzzy-ignore-order-helper str-list)
      "")))


(setq ivy-re-builders-alist '((counsel-company . zenith/ivy-fuzzy-ignore-order)
                              (t . zenith/ivy-cregexp-ignore-order)))

;; amx
;; depednecies: s
(require 'amx)
(setq amx-save-file (expand-file-name "amx-items" zenith-emacs-local-dir))
(amx-mode)

(provide 'init-ivy)
;;; init-ivy.el ends here
