;;; init-company.el --- completion configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; company-mode
(setq company-idle-delay 0
      company-minimum-prefix-length 1
      company-tooltip-limit 10
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-dabbrev-code-other-buffers t
      company-tooltip-align-annotations t
      company-require-match 'never
      company-global-modes
      '(not erc-mode message-mode help-mode gud-mode eshell-mode)
      company-frontends
      '(company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

(require 'company)

;; company-box
;; dependencies: dash dash-functional company
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)
(setq company-box-show-single-candidate t
      company-box-doc-enable nil ; Use C-h to manually show doc
      company-box-backends-colors nil
      company-box-max-candidates 50
      company-box-icons-alist 'company-box-icons-all-the-icons
      company-box-icons-functions
      '(+company-box-icons--elisp +company-box-icons--tabnine +company-box-icons--nox)
      company-box-icons-all-the-icons
      `((Unknown       . ,(all-the-icons-material "find_in_page"             :height 0.8 :face 'all-the-icons-purple))
        (Text          . ,(all-the-icons-material "text_fields"              :height 0.8 :face 'all-the-icons-green))
        (Method        . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
        (Function      . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
        (Constructor   . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
        (Field         . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
        (Variable      . ,(all-the-icons-material "adjust"                   :height 0.8 :face 'all-the-icons-blue))
        (Class         . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))
        (Interface     . ,(all-the-icons-material "settings_input_component" :height 0.8 :face 'all-the-icons-red))
        (Module        . ,(all-the-icons-material "view_module"              :height 0.8 :face 'all-the-icons-red))
        (Property      . ,(all-the-icons-material "settings"                 :height 0.8 :face 'all-the-icons-red))
        (Unit          . ,(all-the-icons-material "straighten"               :height 0.8 :face 'all-the-icons-red))
        (Value         . ,(all-the-icons-material "filter_1"                 :height 0.8 :face 'all-the-icons-red))
        (Enum          . ,(all-the-icons-material "plus_one"                 :height 0.8 :face 'all-the-icons-red))
        (Keyword       . ,(all-the-icons-material "filter_center_focus"      :height 0.8 :face 'all-the-icons-red))
        (Snippet       . ,(all-the-icons-material "short_text"               :height 0.8 :face 'all-the-icons-red))
        (Color         . ,(all-the-icons-material "color_lens"               :height 0.8 :face 'all-the-icons-red))
        (File          . ,(all-the-icons-material "insert_drive_file"        :height 0.8 :face 'all-the-icons-red))
        (Reference     . ,(all-the-icons-material "collections_bookmark"     :height 0.8 :face 'all-the-icons-red))
        (Folder        . ,(all-the-icons-material "folder"                   :height 0.8 :face 'all-the-icons-red))
        (EnumMember    . ,(all-the-icons-material "people"                   :height 0.8 :face 'all-the-icons-red))
        (Constant      . ,(all-the-icons-material "pause_circle_filled"      :height 0.8 :face 'all-the-icons-red))
        (Struct        . ,(all-the-icons-material "streetview"               :height 0.8 :face 'all-the-icons-red))
        (Event         . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-red))
        (Operator      . ,(all-the-icons-material "control_point"            :height 0.8 :face 'all-the-icons-red))
        (TypeParameter . ,(all-the-icons-material "class"                    :height 0.8 :face 'all-the-icons-red))
        (ElispFunction . ,(all-the-icons-material "functions"                :height 0.8 :face 'all-the-icons-red))
        (ElispVariable . ,(all-the-icons-material "check_circle"             :height 0.8 :face 'all-the-icons-blue))
        (ElispFeature  . ,(all-the-icons-material "stars"                    :height 0.8 :face 'all-the-icons-orange))
        (ElispFace     . ,(all-the-icons-material "format_paint"             :height 0.8 :face 'all-the-icons-pink))
        (TabNine       . ,(all-the-icons-material "event"                    :height 0.8 :face 'all-the-icons-green))))

(defun +company-box-icons--tabnine (candidate)
  (when (equal (get-text-property 0 'company-backend candidate) 'company-tabnine)
    'TabNine))

(require 'dash)
(defun +company-box-icons--nox (candidate)
  (-when-let* ((nox-item (get-text-property 0 'nox--lsp-item candidate))
               (kind-num (plist-get nox-item :kind)))
    (alist-get kind-num company-box-icons--lsp-alist)))

(defun +company-box-icons--elisp (candidate)
  (when (derived-mode-p 'emacs-lisp-mode)
    (let ((sym (intern candidate)))
      (cond ((fboundp sym)  'ElispFunction)
            ((boundp sym)   'ElispVariable)
            ((featurep sym) 'ElispFeature)
            ((facep sym)    'ElispFace)))))

(global-company-mode +1)

;; company-tabnine
;; dependencies: company dash s unicode-escape names
(require 'company-tabnine)
(unless (file-exists-p company-tabnine-binaries-folder)
    (company-tabnine-install-binary))

(setq company-tabnine-auto-fallback t
      company-tabnine-max-num-results 10
      company-tabnine-always-trigger nil)

;; Use my own prefix function to replace TabNine's
(defun zenith/get-prefix ()
  (interactive)
  (let ((pattern (find-tag--default))
        (beg (point)))
    (save-excursion
      (when pattern
        (goto-char (+ (point) (length pattern) -1))
        (when (search-backward pattern nil)
          (setq beg (point)))))
    (buffer-substring-no-properties beg (point))))

;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Hash.html
(defun zenith/case-fold-string= (a b)
  (string-equal (upcase a) (upcase b)))

(defun zenith/case-fold-string-hash (a)
  (sxhash-equal (upcase a)))

(define-hash-table-test 'zenith/case-fold
  'zenith/case-fold-string= 'zenith/case-fold-string-hash)

(defvar zenith/company-sort-hashtable (make-hash-table :test 'zenith/case-fold)
  "Hash table for sorting")

(defun zenith/company-compare-string (str1 str2)
  (> (gethash str1 zenith/company-sort-hashtable)
     (gethash str2 zenith/company-sort-hashtable)))

(defun zenith/company-transformer (candidates)
  ;; Sort candidate alphabetically and then show the candidates with same prefix
  ;; first
  (clrhash zenith/company-sort-hashtable)
  (let ((company-prefix (zenith/get-prefix)))
    (dolist (candidate candidates)
      (puthash candidate (or (and company-prefix (fuz-calc-score-skim company-prefix candidate)) 0) zenith/company-sort-hashtable))
    (delete-consecutive-dups (sort candidates #'zenith/company-compare-string))
    ))


(defvar zenith/common-company-backends '(company-capf company-tabnine)
  "Company backends for common buffers")

(defvar-local zenith/local-company-backends zenith/common-company-backends
  "Company backends used for fuzzy matching")
;; The fuzzy matching is not usable in mode other than emacs-list-mode
;; As some backends rely on envirnment to provide the completion.
(defvar-local zenith/fuzzy-matching-prefix ""
  "Prefix for fuzzy-matching")

;; the following code is inspired by company-fuzzy
(defun zenith/company-get-annotation (candidate)
  (zenith/company-get-annotation-by-backend
   candidate
   (get-text-property 0 'company-backend candidate)))

(defun zenith/company-get-annotation-by-backend (candidate backend)
  (if (and candidate
           backend)
      (ignore-errors (funcall backend 'annotation candidate))
    ""))

(defun zenith/company-get-doc (candidate)
  (zenith/company-get-doc-by-backend
   candidate
   (get-text-property 0 'company-backend candidate)))

(defun zenith/company-get-doc-by-backend (candidate backend)
  (if (and candidate
           backend)
      (ignore-errors
       (funcall backend 'doc-buffer candidate))
    ""))

(defun zenith/fuzzy-matching-candidate (candidate)
  (fuz-calc-score-skim zenith/fuzzy-matching-prefix (substring-no-properties candidate)))

(defun zenith/extract-prefix (prefix)
  (cond
   ((or (not prefix) (equal prefix 'stop)) "")
   ((listp prefix) (car prefix))
   (t prefix)))

(defun zenith/get-candidates-fuzzy (backend)
  (save-excursion
    (let* ((prefix (funcall backend 'prefix))
           (zenith/fuzzy-matching-prefix (zenith/extract-prefix prefix))
           (new-prefix (if (equal zenith/fuzzy-matching-prefix "")
                           ""
                         (substring zenith/fuzzy-matching-prefix 0 1)))
           (company-backend backend)
           (candidates))
      (setq candidates (company--preprocess-candidates
                        (company--fetch-candidates new-prefix)))
      (cl-remove-if-not 'zenith/fuzzy-matching-candidate candidates))))

(defun zenith/fuzzy-candidates ()
  (let ((all-candidates '()))
    (dolist (backend zenith/local-company-backends)
      (let ((temp-candidates))
        (ignore-errors
          (if (equal backend 'company-tabnine)
              (let ((company-backend 'company-tabnine))
                (setq temp-candidates (company--preprocess-candidates
                                       (company--fetch-candidates
                                        (funcall backend 'prefix)))))
            (setq temp-candidates (zenith/get-candidates-fuzzy backend)))
          (when temp-candidates
            (mapc (lambda (arg)(put-text-property 0 1 'company-backend backend arg)) temp-candidates)
            (setq all-candidates (append all-candidates temp-candidates))
            ))))
    (zenith/company-transformer all-candidates)))

(defun zenith/fuzzy-matching-backend (command &optional arg &rest ignored)
  "Backend source for fuzzy matching"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'zenith/fuzzy-matching-backends))
    (prefix
     (setq zenith/fuzzy-matching-prefix (zenith/get-prefix))
     (and (not (company-in-string-or-comment))
          (company-grab-symbol)))
    (candidates (zenith/fuzzy-candidates))
    (annotation (zenith/company-get-annotation arg))
    (doc-buffer (zenith/company-get-doc arg))
    (sorted t)))

(defun zenith/set-company-fuzzy-backends ()
  ;; Set backends for company-mode
  (setq company-backends '(zenith/fuzzy-matching-backend)))

(zenith/set-company-fuzzy-backends)

(add-hook 'company-completion-started-hook (lambda (arg) (zenith/temp-no-gc)))
(add-hook 'company-completion-cancelled-hook (lambda (arg) (zenith/restore-gc)))
(add-hook 'company-completion-finished-hook (lambda (arg) (zenith/restore-gc)))

(provide 'init-company)
;;; init-company.el ends here
