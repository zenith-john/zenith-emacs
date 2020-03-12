;;; init-pyim.el --- pyim configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; pyim

(autoload 'pyim-convert-string-at-point "pyim")

(general-define-key "M-/" 'pyim-convert-string-at-point)

(add-hook 'pyim-active-hook 'zenith/pyim-setup)

(defun zenith/pyim-setup ()
  "Initialize for pyim"
  ;; liberime
  (load-file (expand-file-name "liberime/build/liberime.so" zenith-emacs-extension-dir))
  (liberime-start (expand-file-name "/usr/share/rime-data/")
                  (concat zenith-emacs-root-dir "local/pyim/rime/"))
  (liberime-select-schema "luna_pinyin_simp")
  (pyim-isearch-mode 1)
  (add-to-list 'pyim-punctuation-dict '("\\" "、")))

(setq pyim-dcache-directory (concat zenith-emacs-local-dir "pyim/dcache/")
      pyim-page-tooltip 'posframe
      default-input-method "pyim")

(setq pyim-default-scheme 'rime)

(setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode))

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))


(provide 'init-pyim)
;;; init-pyim.el ends here
