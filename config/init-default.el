;;; init-default.el --- better default -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
;;; Emacs core configuration

;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))     ; pretty
(prefer-coding-system 'utf-8)          ; pretty
(setq locale-coding-system 'utf-8)     ; please

(setq-default
 ad-redefinition-action 'accept   ; silence advised function warnings
 apropos-do-all t                 ; make `apropos' more useful
 auto-mode-case-fold nil
 autoload-compute-prefixes nil
 ffap-machine-p-known 'reject     ; don't ping things that look like domain names
 find-file-visit-truename t       ; resolve symlinks when opening files
 idle-update-delay 2              ; update ui less often
 ;; be quiet at startup; don't load or display anything unnecessary
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 ;; History & backup settings (save nothing, that's what git is for)
 auto-save-default nil
 create-lockfiles nil
 history-length 500
 make-backup-files nil  ; don't create backup~ files
 ;; byte compilation
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
 ;; security
 gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
 tls-checktrust gnutls-verify-error
 tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                   ;; compatibility fallbacks
                   "gnutls-cli -p %p %h"
                   "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
 ;; files
 abbrev-file-name             (concat zenith-emacs-local-dir "abbrev.el")
 async-byte-compile-log-file  (concat zenith-emacs-local-dir "async-bytecomp.log")
 auto-save-list-file-name     (concat zenith-emacs-local-dir "autosave")
 backup-directory-alist       (list (cons "." (concat zenith-emacs-local-dir "backup/")))
 desktop-dirname              (concat zenith-emacs-local-dir "desktop")
 desktop-base-file-name       "autosave"
 desktop-base-lock-name       "autosave-lock"
 pcache-directory             (concat zenith-emacs-local-dir "pcache/")
 request-storage-directory    (concat zenith-emacs-local-dir "request")
 server-auth-dir              (concat zenith-emacs-local-dir "server/")
 shared-game-score-directory  (concat zenith-emacs-local-dir "shared-game-score/")
 tramp-auto-save-directory    (concat zenith-emacs-local-dir "tramp-auto-save/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat zenith-emacs-local-dir "tramp-persistency.el")
 url-cache-directory          (concat zenith-emacs-local-dir "url/")
 url-configuration-directory  (concat zenith-emacs-local-dir "url/")
 gamegrid-user-score-file-directory (concat zenith-emacs-local-dir "games/")
 ;; improve performance
 bidi-display-reordering nil)

(setq-default
 large-file-warning-threshold 15000000
 vc-follow-symlinks t
 ;; Save clipboard contents into kill-ring before replacing them
 save-interprogram-paste-before-kill t
 ;; Bookmarks
 bookmark-default-file (concat zenith-emacs-local-dir "bookmarks")
 bookmark-save-flag t
 ;; Formatting
 delete-trailing-lines nil
 fill-column 80
 sentence-end-double-space nil
 word-wrap t
 ;; Scrolling
 hscroll-margin 2
 hscroll-step 1
 scroll-conservatively 1001
 scroll-margin 2
 scroll-preserve-screen-position t
 ;; Whitespace (see `editorconfig')
 indent-tabs-mode nil
 require-final-newline t
 tab-always-indent t
 tab-width 4
 tabify-regexp "^\t* [ \t]+" ; for :retab
 ;; Wrapping
 truncate-lines t
 truncate-partial-width-windows 50)

;; Make yes-or-no y-or-n
(fset 'yes-or-no-p 'y-or-n-p)

(put 'dired-find-alternate-file 'disabled nil)

(show-paren-mode 1)

(setq recentf-save-file (concat zenith-emacs-local-dir "recentf")
      recentf-auto-cleanup 'never
      recentf-max-menu-items 0
      recentf-max-saved-items 300
      recentf-filename-handlers '(file-truename abbreviate-file-name)
      recentf-exclude
      (list #'file-remote-p "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
            "^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$"
            "^/var/folders/.+$"
            ;; ignore private DOOM temp files (but not all of them)
            (lambda (file) (file-in-directory-p file zenith-emacs-local-dir))))
(unless noninteractive
  (add-hook 'kill-emacs-hook #'recentf-cleanup)
  (recentf-mode +1))

(global-auto-revert-mode 1)

(cua-mode 1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; winner-mode
(winner-mode 1)

(provide 'init-default)
;;; init-default.el ends here

