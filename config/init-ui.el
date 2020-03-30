;;; init-ui.el --- ui configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(defvar zenith-font (font-spec :family "Iosevka Term SS09" :size 16 :weight 'semi-bold))
(defvar zenith-unicode-font (font-spec :family "Sarasa Term SC" :weight 'bold))
(setq inhibit-x-resources t)

(defun zenith/init-font ()
  (add-to-list 'default-frame-alist `(font . ,(font-xlfd-name zenith-font)))
  (set-fontset-font t 'unicode zenith-unicode-font nil 'prepend))

(zenith/init-font)

;; darkroom
(zenith/autoload '(darkroom-mode darkroom-tentative-mode) "darkroom")
(setq darkroom-text-scale-increase 1)

;; doom-themes
(require 'doom-themes)
(load-theme 'doom-one t)

;; awesome-tray
(require 'awesome-tray)
(awesome-tray-mode 1)

(defun zenith/org-clock-info ()
  (if (org-clocking-p)
      (format "[%s] (%s)"
              (org-duration-from-minutes (org-clock-get-clocked-time))
              org-clock-heading)
    ("")))

(defface zenith/org-clock-face
  '((t
     :foreground "#bc005e" :bold t))
  "zenith/org-clock face"
  :group 'awesome-tray)

;; Redefine advice to make sure no duplicate info
(defun awesome-tray-get-echo-format-string (message-string)
  (let* ((tray-info (awesome-tray-build-info))
         (blank-length (- (awesome-tray-get-frame-width) (string-width tray-info) (string-width message-string) awesome-tray-info-padding-right))
         (empty-fill-string (make-string (max 0 (- (awesome-tray-get-frame-width) (string-width tray-info) awesome-tray-info-padding-right)) ?\ ))
         (message-fill-string (make-string (max 0 (- (awesome-tray-get-frame-width) (string-width message-string) (string-width tray-info) awesome-tray-info-padding-right)) ?\ )))
    (prog1
        (cond
         ;; Fill message's end with whitespace to keep tray info at right of minibuffer.
         ((> blank-length 0)
          (concat message-string message-fill-string tray-info))
         ;; Fill empty whitespace if new message contain duplicate tray-info (cause by move mouse on minibuffer window).
         ((or (and awesome-tray-last-tray-info
                   message-string
                   (string-suffix-p awesome-tray-last-tray-info message-string))
              (eq major-mode 'org-agenda-mode)) ; org-agenda-mode has strange buffer-name behavior
          (concat empty-fill-string tray-info))
         ;; Don't fill whitepsace at end of message if new message is very long.
         (t
          (concat message-string "\n" empty-fill-string tray-info)))
      ;; Record last tray information.
      (setq awesome-tray-last-tray-info tray-info))))

(defun awesome-tray-message-advice (old-message &rest arguments)
  (unless (ignore-errors
            (cond
             ;; Don't wrap tray info if `awesome-tray-active-p' is nil.
             ((not awesome-tray-active-p)
              (apply old-message arguments))
             ;; Don't wrap awesome-tray info if variable `inhibit-message' is non-nil.
             (inhibit-message
              (apply old-message arguments))
             ;; Just flush tray info if message string is empty.
             ((not (car arguments))
              (apply old-message arguments)
              (awesome-tray-flush-info))
             ;; Otherwise, wrap message string with tray info.
             (t
              (apply old-message '("")) ; Ugly trick, force flushing the message area
              (apply old-message "%s" (cons (awesome-tray-get-echo-format-string (apply 'format arguments)) '()))
              ))
            ;; Return t if everything is okay.
            t)
    (apply old-message "Sorry, something error in awesome-tray.")
    ))

(add-to-list 'awesome-tray-module-alist
             '("clock" . (zenith/org-clock-info zenith/org-clock-face)))

(setq awesome-tray-active-modules
     '("evil" "location" "clock" "git" "buffer-name" "mode-name"))

(global-hl-line-mode 1)

;; goto-line-preview
(autoload 'goto-line-preview "goto-line-preview")
(general-def [remap goto-line] 'goto-line-preview)

(provide 'init-ui)
;;; init-ui.el ends here
