;;; init-company-backend.el --- zenith company backend -*- lexical-binding: t; -*-

;;; Commentary:

;;; A company-backend integrate multiple backends, solving the problem of the
;;; :with and :separate keywords not working as expected

;;; Code:

(require 'company)
(require 'fuz)
(unless (require 'fuz-core nil t)
  (fuz-build-and-load-dymod))

(defvar company-combine-common-backends '(company-capf)
  "Company backends for common buffers")

(defvar-local company-combine-local-backends company-combine-common-backends
  "Company backends used for matching")
;; The matching is not usable in mode other than emacs-list-mode
;; As some backends rely on environment to provide the completion.
(defvar-local company-combine-matching-prefix ""
  "Prefix for matching")

;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Hash.html
(defun company-combine-case-fold-string= (a b)
  (string-equal (upcase a) (upcase b)))

(defun company-combine-case-fold-string-hash (a)
  (sxhash-equal (upcase a)))

(define-hash-table-test 'company-combine-case-fold-test-fn
  'company-combine-case-fold-string= 'company-combine-case-fold-string-hash)

(defvar company-combine-sort-hashtable (make-hash-table :test 'company-combine-case-fold-test-fn)
  "Hash table for sorting")

(defun company-combine-compare-string (str1 str2)
  (> (gethash str1 company-combine-sort-hashtable)
     (gethash str2 company-combine-sort-hashtable)))

(defun company-combine-transformer (candidates)
  ;; Sort candidate alphabetically and then show the candidates with same prefix
  ;; first
  (clrhash company-combine-sort-hashtable)
  (dolist (candidate candidates)
    (puthash candidate
             (or (and company-combine-matching-prefix
                      (fuz-calc-score-skim company-combine-matching-prefix candidate)) 0)
             company-combine-sort-hashtable))
  (delete-consecutive-dups (sort candidates #'company-combine-compare-string)))

(defun company-combine-get-prefix ()
  (interactive)
  (let ((prefix "") (new-prefix) (len 200) (new-len 0)) ; I think no word is longer than 200
    (dolist (backend company-combine-local-backends)
      (setq new-prefix (funcall backend 'prefix))
      (when new-prefix
        (cond
         ((eq new-prefix 'stop) (setq new-len 200))
         ((cdr-safe new-prefix)
          (setq new-len (cdr new-prefix)))
         (t (setq new-len (length new-prefix))))
        (when (< new-len len)
          (setq prefix new-prefix
                len new-len))))
    (if (< len 200)
        prefix
      nil)))

;; the following code is inspired by company-fuzzy
(defun company-combine-get-annotation (candidate)
  (company-combine-get-annotation-by-backend
   candidate
   (get-text-property 0 'company-backend candidate)))

(defun company-combine-get-annotation-by-backend (candidate backend)
  (if (and candidate
           backend)
      (ignore-errors (funcall backend 'annotation candidate))
    ""))

(defun company-combine-get-doc (candidate)
  (company-combine-get-doc-by-backend
   candidate
   (get-text-property 0 'company-backend candidate)))

(defun company-combine-get-doc-by-backend (candidate backend)
  (if (and candidate
           backend)
      (ignore-errors
       (funcall backend 'doc-buffer candidate))
    ""))

(defun company-combine-matching-candidate (candidate)
  (fuz-calc-score-skim company-combine-matching-prefix (substring-no-properties candidate)))

(defun company-combine-get-candidates (backend)
  (save-excursion
    (when-let* ((prefix (funcall backend 'prefix))
                (company-backend backend)
                (candidates t))
      (setq candidates (company--preprocess-candidates
                        (company--fetch-candidates prefix)))
      (cl-remove-if-not 'company-combine-matching-candidate candidates))))

(defun company-combine-candidates ()
  (let ((all-candidates '()))
    (dolist (backend company-combine-local-backends)
      (let ((temp-candidates))
        (ignore-errors
          (setq temp-candidates (company-combine-get-candidates backend))
          (when temp-candidates
            (mapc (lambda (arg)(put-text-property 0 1 'company-backend backend arg)) temp-candidates)
            (setq all-candidates (append all-candidates temp-candidates))))))
    (company-combine-transformer all-candidates)))

(defun company-combine-post-completion (result)
  "Do post completion depend on backends"
  (let ((backend (get-text-property 0 'company-backend result)))
    (if (and backend
             result)
        (ignore-errors
          (funcall backend 'post-completion result))
      nil)))

;; Advice `company-finish' to deal with the prefix correctly.
(defadvice company-finish (before company-combine-update-prefix (candidate) activate)
  "Get prefix right depending on the backend"
  (when-let ((backend (get-text-property 0 'company-backend candidate)))
    (setq company-prefix
          (funcall backend 'prefix))))

(defun company-combine (command &optional arg &rest ignored)
  "Company combine backend"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-combine))
    (prefix
     (and (not (company-in-string-or-comment))
          (setq company-combine-matching-prefix (company-combine-get-prefix))))
    (candidates (company-combine-candidates))
    (annotation (company-combine-get-annotation arg))
    (doc-buffer (company-combine-get-doc arg))
    (post-completion (company-combine-post-completion arg))
    (sorted t)
    (no-cache t)))

(defun company-combine-set-company-backends ()
  ;; Set backends for company-mode
  (setq company-backends '(company-combine)))

(provide 'init-company-backend)
