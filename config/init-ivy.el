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
      ivy-use-virtual-buffers t
      ;; ...but if that ever changes, show their full path
      ivy-virtual-abbreviate 'full
      ;; don't quit minibuffer on delete-error
      ivy-on-del-error-function nil
      ;; enable ability to select prompt (alternative to `ivy-immediate-done')
      ivy-use-selectable-prompt t)

(general-def ivy-mode-map
  [remap switch-to-buffer]              #'ivy-switch-buffer
  [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window
  [remap imenu-anywhere]                #'ivy-imenu-anywhere)

(general-def ivy-minibuffer-map [escape] #'keyboard-escape-quit)

;; Don't show annoying helpful buffer in buffer selection list
(add-to-list 'ivy-ignore-buffers "\*helpful")

(ivy-mode +1)

(require 'ivy-prescient)
(setq ivy-prescient-retain-classic-highlighting t)
(ivy-prescient-mode)

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

(general-def
  [remap apropos]                  #'counsel-apropos
  [remap bookmark-jump]            #'counsel-bookmark
  [remap describe-face]            #'counsel-faces
  [remap describe-function]        #'counsel-describe-function
  [remap describe-variable]        #'counsel-describe-variable
  [remap describe-bindings]        #'counsel-descbinds
  [remap set-variable]             #'counsel-set-variable
  [remap execute-extended-command] #'counsel-M-x
  [remap find-file]                #'counsel-find-file
  [remap find-library]             #'counsel-find-library
  [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
  [remap imenu]                    #'counsel-imenu
  [remap recentf-open-files]       #'counsel-recentf
  [remap org-capture]              #'counsel-org-capture
  [remap swiper]                   #'counsel-grep-or-swiper
  "M-y"                            #'counsel-yank-pop)
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
   ("f" find-file-other-window "other window")
   ("F" find-file-other-frame "other frame")
   ("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory)))) "insert relative path")
   ("P" (lambda (path) (with-ivy-window (insert path))) "insert absolute path")
   ("l" (lambda (path) "Insert org-link with relative path"
          (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory))))) "insert org-link (rel. path)")
   ("L" (lambda (path) "Insert org-link with absolute path"
          (with-ivy-window (insert (format "[[%s]]" path)))) "insert org-link (abs. path)")))


;; fuz.el
(require 'fuz)
(unless (require 'fuz-core nil t)
  (fuz-build-and-load-dymod))

;; ;; ivy-fuz.el
;; ;; dependencies: fuz ivy
(require 'ivy-fuz)

;; Less sort limit for better performance as the sort process is the bottleneck
;; of fuz matching.
(setq ivy-fuz-sort-limit 100)
(setq ivy-sort-matches-functions-alist '((helpful-variable)
                                         (helpful-callable)
                                         (t . ivy-fuz-sort-fn))
      ivy-re-builders-alist '((counsel-rg . ivy--regex)
                              (t . ivy-fuz-regex-fuzzy)))

(provide 'init-ivy)
;;; init-ivy.el ends here
