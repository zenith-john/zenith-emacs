;;; init-wsl. --- WSL configuration for emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
;;; The following convert functions are from https://www.emacswiki.org/emacs/wsl-path.el
(defvar wsl-path-prefix "/mnt/"
  "Prefix for the WSL mount points.")

(defun resolve-file-ignoring-case (file dir)
  "Find a entry in DIR whose name matches FILE to within case."
  (cl-find (downcase file) (directory-files dir)
           :key 'downcase
           :test 'string-equal))

(defun resolve-path-ignoring-case (path)
  "Attempt to find a file ignoring case.  PATH must be an simple
absolute path like one returned from SUBSTITUTE-IN-FILE-NAME.
The file must exist for this to be meaningful, otherwise it will
simply return whatever you input."
  (if (file-exists-p path)
      path
    (let* ((filename "/"))
      (cl-loop for file in (split-string path "/" t)
               for guess = (concat filename file)
               do
               (if (file-exists-p guess)
                   (setf filename (concat guess "/"))
                 (let ((real-file (resolve-file-ignoring-case file filename)))
                   (if real-file
                       (setf filename (concat filename real-file "/"))
                     ;; If all else fails, leave it unchanged
                     (setf filename (concat filename file "/")))))
               finally (return
                        ;; Remove trailing slash if not on input
                        (if (equal ?/ (aref path (- (length path) 1)))
                            filename
                          (substring filename 0 (- (length filename) 1))))))))

(defun wsl-path-run-real-handler (operation args)
  "Run OPERATION with ARGS."
  (let ((inhibit-file-name-handlers
         (append '(wsl-path-map-drive-hook-function)
                 (and (eq inhibit-file-name-operation operation)
                      inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defconst wsl-path-style1-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\)\\\\")
(defconst wsl-path-style2-regexp "\\`\\(.*/\\)?\\([a-zA-Z]:\\)/")

;; We cannot assume that NAME matched wsl-path-style1-regexp nor
;; wsl-path-style2-regexp because this function could be called with
;; either argument to `expand-file-name', but only one argument to
;; `expand-file-name' may have matched a regexp.  For example,
;; `(expand-file-name ".." "c:/")' will trigger `(wsl-path-convert-file-name
;; "..")' and `(wsl-path-convert-file-name "c:/")' to be called.
(defun wsl-path-convert-file-name (name)
  "Convert file NAME to WSL style.
`x:/' to `/mnt/x/'."
  (let ((inhibit-file-name-handlers
         (append '(wsl-path-map-drive-hook-function)
                 (and (eq inhibit-file-name-operation
                          'substitute-in-file-name)
                      inhibit-file-name-handlers)))
        (inhibit-file-name-operation 'substitute-in-file-name))
    (cond ((string-match wsl-path-style1-regexp name)
           (let ((filename
                  (replace-match (concat wsl-path-prefix
                                         (downcase (substring (match-string 2 name) 0 1)))
                                 t nil name 2)))
             (while (string-match "\\\\" filename)
               (setq filename
                     (replace-match "/" t nil filename)))
             (resolve-path-ignoring-case
              (substitute-in-file-name filename))))
          ((string-match wsl-path-style2-regexp name)
           (resolve-path-ignoring-case
            (substitute-in-file-name
             (replace-match (concat wsl-path-prefix
                                    (downcase (substring (match-string 2 name) 0 1)))
                            t nil name 2))))

          (t name))))

(with-eval-after-load 'org-download
  (setq org-download-screenshot-method
        "i_view64.exe /capture=4 /convert=\"D:\\\\screenshot.png\"; mv /mnt/d/screenshot.png %s")

  (defun org-download-yank ()
    "Call `org-download-image' with current kill."
    (interactive)
    (let ((k (wsl-path-convert-file-name (current-kill 0))))
      (unless (url-type (url-generic-parse-url k))
        ;; (user-error "Not a URL: %s" k)
        (setq k (concat "file://" k))
        )
      (org-download-image
       (replace-regexp-in-string
        "\n+$" "" k))))

  (defun org-download-clipboard (&optional basename)
    "Capture the image from the clipboard and insert the resulting file."
    (interactive)
    (let ((org-download-screenshot-method
           (cl-case system-type
             (gnu/linux
              (if (executable-find "convert.exe")
                  "convert.exe clipboard: %s"
                (if (string= "wayland" (getenv "XDG_SESSION_TYPE"))
                    (if (executable-find "wl-paste")
                        "wl-paste -t image/png > %s"
                      (user-error
                       "Please install the \"wl-paste\" program included in wl-clipboard"))
                  (if (executable-find "xclip")
                      "xclip -selection clipboard -t image/png -o > %s"
                    (user-error
                     "Please install the \"xclip\" program")))))
             ((windows-nt cygwin)
              (if (executable-find "convert")
                  "convert clipboard: %s"
                (user-error
                 "Please install the \"convert\" program included in ImageMagick")))
             ((darwin berkeley-unix)
              (if (executable-find "pngpaste")
                  "pngpaste %s"
                (user-error
                 "Please install the \"pngpaste\" program from Homebrew."))))))
      (org-download-screenshot basename))))

(provide 'init-wsl)
;;; init-wsl.el ends here
