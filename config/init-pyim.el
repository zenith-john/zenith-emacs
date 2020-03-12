;;; init-pyim.el --- pyim configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; pyim

(require 'pyim)

(general-define-key "M-/" 'pyim-convert-string-at-point)
(setq pyim-dcache-directory (concat zenith-emacs-local-dir "pyim/dcache/")
      pyim-page-tooltip 'posframe
      default-input-method "pyim")
;; liberime
(load-file (expand-file-name "liberime/build/liberime.so" zenith-emacs-extension-dir))
(liberime-start (expand-file-name "/usr/share/rime-data/")
                (concat zenith-emacs-root-dir "local/pyim/rime/"))
(liberime-select-schema "luna_pinyin_simp")
(setq pyim-default-scheme 'rime)

(pyim-isearch-mode 1)
(add-to-list 'pyim-punctuation-dict '("\\" "、"))
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode))

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))


(provide 'init-pyim)
;;; init-pyim.el ends here
