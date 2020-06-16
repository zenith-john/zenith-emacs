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
      ivy-fixed-height-minibuffer t
      projectile-completion-system 'ivy
      ;; Don't use ^ as initial input
      ivy-initial-inputs-alist nil
      ;; highlight til EOL
      ivy-format-function #'ivy-format-function-line
      ;; enable magic slash on non-match
      ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
      ;; don't show recent files in switch-buffer
      ivy-use-virtual-buffers nil
      ;; ...but if that ever changes, show their full path
      ivy-virtual-abbreviate 'full
      ;; don't quit minibuffer on delete-error
      ivy-on-del-error-function nil
      ;; enable ability to select prompt (alternative to `ivy-immediate-done')
      ivy-use-selectable-prompt t)

;; Don't show annoying helpful buffer in buffer selection list
(add-to-list 'ivy-ignore-buffers "\*helpful")

(ivy-mode +1)

;; ivy-posframe
;; dependencies: ivy posframe
(require 'ivy-posframe)

(defun zenith/ivy-posframe-get-size ()
  "Set size of by the posframe"
  (list
   :height (max (+ 1 ivy-height) ivy-posframe-height)
   :width (round (* 0.5 (frame-width)))
   :min-height (max (+ 1 ivy-height) ivy-posframe-height)
   :min-width (max 80 (round (* 0.5 (frame-width))))))

;; Set color for ivy-posframe
(with-eval-after-load 'doom-themes
  (let ((bg-color (face-background 'default nil))
        (fg-color (face-foreground 'default nil)))
    (set-face-attribute 'ivy-posframe nil
                        :background (doom-lighten bg-color 0.05)
                        :foreground (doom-lighten fg-color 0.05))))

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
      (mapcar 'zenith/ivy-cregexp-helper (split-string str))
      "")))

(setq ivy-re-builders-alist '((t . zenith/ivy-cregexp-ignore-order)))

(add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn))

;; amx
;; depednecies: s
(require 'amx)
(setq amx-save-file (expand-file-name "amx-items" zenith-emacs-local-dir))
(amx-mode)

(provide 'init-ivy)
;;; init-ivy.el ends here
