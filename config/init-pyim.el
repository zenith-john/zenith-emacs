;;; init-pyim.el --- pyim configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; pyim

(autoload 'pyim-convert-string-at-point "pyim")

(with-eval-after-load 'pyim
  ;; liberime
  (load-file (expand-file-name "liberime/build/liberime.so" zenith-emacs-extension-dir))
  (liberime-start (expand-file-name "/usr/share/rime-data/")
                  (concat zenith-emacs-root-dir "pyim/rime/"))
  (liberime-sync-user-data)
  (liberime-select-schema "luna_pinyin_simp")
  (pyim-isearch-mode 1)
  (add-to-list 'pyim-punctuation-dict '("\\" "、")))

(setq pyim-dcache-directory (concat zenith-emacs-local-dir "pyim/dcache/")
      pyim-page-tooltip 'posframe
      default-input-method "pyim"
      pyim-liberime-search-limit 100)

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
