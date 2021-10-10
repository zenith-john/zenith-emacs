;;; cron.el --- Auto update some information constantly -*- lexical-binding: t; -*-

;;; Commentary:
;;; This file is added to cron to run every one hour
;;; 1. Update the ics file from the agenda.
;;; 2. Update the latex image preview in background.

(defvar zenith-emacs-root-dir user-emacs-directory)
(defvar zenith-emacs-extension-dir (expand-file-name "extensions/" zenith-emacs-root-dir))
(defvar zenith-emacs-config-dir (expand-file-name "config/" zenith-emacs-root-dir))
(defvar zenith-emacs-local-dir (expand-file-name "local/" zenith-emacs-root-dir))

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(let (
      ;; Reduce GC to speed up initialization.
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      ;; Do not load major mode
      (file-name-handler-alist nil))

  (require 'cl-lib)
  ;; From https://emacs-china.org/t/topic/3931/2
  (defun eh-hack-load-path ()
    ;; Delete buildin org's PATH
    (setq load-path
          (cl-remove-if
           #'(lambda (path)
               (string-match "lisp/org$" path))
           load-path))
    ;; Demove property lists to defeat cus-load and remove autoloads
    (mapatoms
     #'(lambda (sym)
         (let ((sym-name (symbol-name sym)))
           (when (string-match "^\\(org\\|ob\\|ox\\)-?" sym-name)
             (setplist sym nil)
             (when (autoloadp sym)
               (unintern sym)))))))

  (eh-hack-load-path)
  ;; Load new org-mode rather than system one
  (add-subdirs-to-load-path (expand-file-name "org-mode/" zenith-emacs-extension-dir))

  (require 'org)
  (require 'ox-icalendar)
  (require 'org-id)

  (defvar org-directory "~/Dropbox/")
  (defvar org-agenda-files '("~/Dropbox/"))
  (defvar zenith/note-directory (expand-file-name "~/Documents/Notes/"))

  (setq-default
      org-id-track-globally t
      org-id-link-to-org-use-id t
      org-id-locations-file (expand-file-name ".org-id-locations" zenith-emacs-local-dir)
      org-id-extra-files (directory-files-recursively zenith/note-directory ".*\\.org"))
  (org-id-update-id-locations)

  ;;; Update icalendar file

  (setq org-icalendar-combined-agenda-file (expand-file-name "~/Dropbox/agenda.ics")
        org-icalendar-include-todo t
        org-icalendar-use-deadline '(event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(event-if-not-todo todo-start)
        org-icalendar-alarm-time 15
        org-icalendar-store-UID t
        org-agenda-default-appointment-duration nil)
  (org-icalendar-combine-agenda-files)

  ;;; Update latex preview

  ;; The foreground and background color has to be manually assigned.
  (setq-default
   org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (setq-default
   org-preview-latex-image-directory (concat zenith-emacs-local-dir "org-latex/")
   org-format-latex-options
   (plist-put (plist-put org-format-latex-options
                         :background
                         "#292D3E")
              :foreground
              "#EEFFFF"))

  (defun zenith/org--get-diplay-dpi-advice (orig-fn)
    "Make `org--get-display-dpi' work for non graphic mode"
    (if (not (display-graphic-p))
        ;; manually set default dpi
        96
      (funcall orig-fn)))

  (defun zenith/color-values-advice (orig-fn color &optional frame)
    "Make `color-values' work temporarily in terminal condition."
    (if (length= color 7)
        (mapcar (lambda (str)
              (* (string-to-number str 16) 256))
                `(,(substring color 1 3) ,(substring color 3 5) ,(substring color 5 7)))
      (apply orig-fn color frame)))

  ;; Modified from `org-format-latex'

  ;; Remove the modification of the buffer and make dvipng always generate
  ;; 'forbuffer png file
  (defun zenith/org-format-latex
      (prefix &optional beg end dir overlays msg forbuffer processing-type)
    "Replace LaTeX fragments with links to an image.

The function takes care of creating the replacement image.

Only consider fragments between BEG and END when those are
provided.

When optional argument OVERLAYS is non-nil, display the image on
top of the fragment instead of replacing it.

PROCESSING-TYPE is the conversion method to use, as a symbol.

Some of the options can be changed using the variable
`org-format-latex-options', which see."
    (when (and overlays (fboundp 'clear-image-cache)) (clear-image-cache))
    (unless (eq processing-type 'verbatim)
      (let* ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
             (cnt 0)
             checkdir-flag)
        (goto-char (or beg (point-min)))
        ;; Optimize overlay creation: (info "(elisp) Managing Overlays").
        (when (and overlays (memq processing-type '(dvipng imagemagick)))
          (overlay-recenter (or end (point-max))))
        (while (re-search-forward math-regexp end t)
          (unless (and overlays
                       (eq (get-char-property (point) 'org-overlay-type)
                           'org-latex-overlay))
            (let* ((context (org-element-context))
                   (type (org-element-type context)))
              (when (memq type '(latex-environment latex-fragment))
                (let ((block-type (eq type 'latex-environment))
                      (value (org-element-property :value context))
                      (beg (org-element-property :begin context))
                      (end (save-excursion
                             (goto-char (org-element-property :end context))
                             (skip-chars-backward " \r\t\n")
                             (point))))
                  (cond
                   ((eq processing-type 'mathjax)
                    ;; Prepare for MathJax processing.
                    (if (not (string-match "\\`\\$\\$?" value))
                        (goto-char end)))
                   ((eq processing-type 'html)
                    (goto-char beg))
                   ((assq processing-type org-preview-latex-process-alist)
                    ;; Process to an image.
                    (cl-incf cnt)
                    (goto-char beg)
                    (let* ((processing-info
                            (cdr (assq processing-type org-preview-latex-process-alist)))
                           (face (face-at-point))
                           ;; Get the colors from the face at point.
                           (fg
                            (let ((color (plist-get org-format-latex-options
                                                    :foreground)))
                              (if forbuffer
                                  (cond
                                   ((eq color 'auto)
                                    (face-attribute face :foreground nil 'default))
                                   ((eq color 'default)
                                    (face-attribute 'default :foreground nil))
                                   (t color))
                                color)))
                           (bg
                            (let ((color (plist-get org-format-latex-options
                                                    :background)))
                              (if forbuffer
                                  (cond
                                   ((eq color 'auto)
                                    (face-attribute face :background nil 'default))
                                   ((eq color 'default)
                                    (face-attribute 'default :background nil))
                                   (t color))
                                color)))
                           (hash (sha1 (prin1-to-string
                                        (list org-format-latex-header
                                              org-latex-default-packages-alist
                                              org-latex-packages-alist
                                              org-format-latex-options
                                              'forbuffer value fg bg))))
                           (imagetype (or (plist-get processing-info :image-output-type) "png"))
                           (absprefix (expand-file-name prefix dir))
                           (linkfile (format "%s_%s.%s" prefix hash imagetype))
                           (movefile (format "%s_%s.%s" absprefix hash imagetype))
                           (sep (and block-type "\n\n"))
                           (link (concat sep "[[file:" linkfile "]]" sep))
                           (options
                            (org-combine-plists
                             org-format-latex-options
                             `(:foreground ,fg :background ,bg))))
                      (when msg (message msg cnt))
		              (unless checkdir-flag ; Ensure the directory exists.
		                (setq checkdir-flag t)
		                (let ((todir (file-name-directory absprefix)))
			              (unless (file-directory-p todir)
			                (make-directory todir t))))
		              (unless (file-exists-p movefile)
		                (org-create-formula-image
		                 value movefile options 'forbuffer processing-type)))
                    (goto-char end))
		           ((eq processing-type 'mathml)
		            ;; Process to MathML.
		            (unless (org-format-latex-mathml-available-p)
		              (user-error "LaTeX to MathML converter not configured"))
		            (cl-incf cnt)
		            (when msg (message msg cnt))
		            (goto-char end))
		           (t
		            (error "Unknown conversion process %s for LaTeX fragments"
			               processing-type)))))))))))

  (defun zenith/org--latex-preview-region (beg end)
    "Preview LaTeX fragments between BEG and END.
BEG and END are buffer positions."
    (let ((file (buffer-file-name (buffer-base-buffer))))
      (save-excursion
        (zenith/org-format-latex
         (concat org-preview-latex-image-directory "org-ltximg")
         beg end
         ;; Emacs cannot overlay images from remote hosts.  Create it in
         ;; `temporary-file-directory' instead.
         (if (or (not file) (file-remote-p file))
             temporary-file-directory
           default-directory)
         nil nil nil org-preview-latex-default-process))))

  (defun zenith/org-latex-preview-file (file)
    "Create the latex preview cache for FILE."
    (message "Produce latex preview for %s" file)
    (find-file file)
    (zenith/org--latex-preview-region (point-min) (point-max)))

  (defun zenith/org-latex-preview-cache ()
    "Generate the cache for `org-latex-preview'"
    (interactive)
    (advice-add 'org--get-display-dpi :around 'zenith/org--get-diplay-dpi-advice)
    (advice-add 'color-values :around 'zenith/color-values-advice)
    (dolist (file (directory-files zenith/note-directory t "\\.org\\'"))
      (zenith/org-latex-preview-file file))
    (advice-remove 'org--get-display-dpi 'zenith/org--get-diplay-dpi-advice)
    (advice-remove 'color-values 'zenith/color-values-advice))

  (zenith/org-latex-preview-cache))

(provide 'cron)
;;; cron.el ends here
