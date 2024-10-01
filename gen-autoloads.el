;;; gen-autoloads.el --- generate autoloads file -*- lexical-binding: t; -*-

;;; Commentary:

(defun cm/find-subdir-recursively (dir)
  "Find all subdirectories in DIR.

Dot-directories and directories contain `.nosearch' will be skipped."
  (thread-last (directory-files dir nil)
    (cl-remove-if (lambda (f)
                    (string-prefix-p "." f)))
    (mapcar (lambda (d) (expand-file-name d dir)))
    (cl-remove-if-not #'file-directory-p)
    (cl-remove-if (lambda (d)
                    (file-exists-p (expand-file-name ".nosearch"
                                                     d))))))

(defun cm/find-el-file-recursively (dir)
  "Find all `.el' files in DIR and its subdirectories."
  (let ((elfiles (directory-files dir t "\\.el\\'"))
        (subdir (cm/find-subdir-recursively dir)))
    (nconc elfiles
           (mapcan #'cm/find-el-file-recursively subdir))))

(defun cm/generate-autoloads (&optional dir target)
  "Generate autoload files recursively for all package in DIR to file TARGET.

If DIR is omitted, use `cm/site-lisp-directory' as DIR, if target is ommitted
use `cm/autoloads-file' as TARGET."
  (interactive)
  (require 'autoload)
  (let* ((target (or target cm/autoloads-file))
         (dir (or dir cm/site-lisp-directory))
         (generated-autoload-file target))
    (with-temp-file target
      (dolist (f (cm/find-el-file-recursively dir))
        (let ((generated-autoload-load-name (file-name-sans-extension f)))
          (autoload-generate-file-autoloads f (current-buffer))))
      (insert (string-join `(,(char-to-string ?\C-l)
                             ";; Local Varibles:"
                             ";; version-control: never"
                             ";; no-byte-compile: t"
                             ";; no-update-autoloads: t"
                             ";; coding: utf-8"
                             ";; End:"
                             ,(format ";;; %s ends here"
                                      (file-name-nondirectory target)))
                           "\n")))
    (load target :no-error :no-message)))

(cm/generate-autoloads "extensions" "loaddefs.el")
;;; gen-autoloads.el ends here
