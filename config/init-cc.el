;;; init-cc.el --- cc configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; The file's majority is from
;;; https://github.com/hlissner/doom-emacs/blob/develop/modules/lang/cc/config.el
;;; The doom dependence is removed. So it is less powerful.

;;; Code:
(setq-default c-basic-offset tab-width
              c-backspace-function #'delete-backward-char
              c-default-style "microsoft")

;; The plusses in c++-mode can be annoying to search for ivy/helm (which reads
;; queries as regexps), so we add these for convenience.
(defalias 'cpp-mode 'c++-mode)
(defvaralias 'cpp-mode-map 'c++-mode-map)

  ;;; Better fontification (also see `modern-cpp-font-lock')
(add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)

;; Custom style, base from llvm Microsoft
;; help with c-guess-no-install and c-guess-view
(c-add-style "microsoft"
             '("linux"
               (c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (access-label . *)      ; Guessed value
                (block-close . 0)       ; Guessed value
                (brace-list-close . 0)  ; Guessed value
                (brace-list-entry . 0)  ; Guessed value
                (brace-list-intro . +)  ; Guessed value
                (brace-list-open . 0)   ; Guessed value
                (class-close . 0)       ; Guessed value
                (class-open . 0)        ; Guessed value
                (defun-block-intro . +) ; Guessed value
                (defun-close . 0)       ; Guessed value
                (defun-open . 0)        ; Guessed value
                (else-clause . 0)       ; Guessed value
                (inclass . +)           ; Guessed value
                (statement . 0)             ; Guessed value
                (statement-block-intro . +) ; Guessed value
                (substatement . +)      ; Guessed value
                (substatement-open . 0) ; Guessed value
                )))

;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
;; The result isn't very intelligent (causes redundant characters), so just do
;; it ourselves.
(general-def c++-mode-map "<" nil ">" nil)
(with-eval-after-load 'smartparens-config
  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))

;; modern-cpp-font-lock
(autoload 'modern-c++-font-lock-mode "modern-cpp-font-lock")
(add-hook 'c++-mode-hook 'modern-c++-font-lock-mode)

;; QT keywords
(setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                  "\\|protected slot\\|private\\|private slot"
                                  "\\)\\>"))

;; QT pro
(require 'qt-pro-mode)
(add-to-list 'auto-mode-alist '("\\.pr[io]$" . qt-pro-mode))

(defun zenith/c-flycheck-hook ()
  (flycheck-mode 1)
  (require 'flycheck-clang-tidy)
  (setq-local flycheck-checker 'c/c++-clang-tidy))
(add-hook 'c-mode-common-hook 'zenith/c-flycheck-hook)

(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-directories ".ccls-cache")
  (add-to-list 'projectile-project-root-files-bottom-up ".ccls-root")
  (add-to-list 'projectile-project-root-files-top-down-recurring "compile_commands.json"))

(provide 'init-cc)
;;; init-cc.el ends here
