;;; init-pyim.el --- pyim configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(use-package pyim
  :general ("M-/" 'pyim-convert-string-at-point)
  :demand t
  :init
  (setq pyim-dcache-directory (concat zenith-emacs-local-dir "pyim/dcache/")
        pyim-page-tooltip 'posframe
        default-input-method "pyim")
  :config
  (pyim-isearch-mode 1)
  (add-to-list 'pyim-punctuation-dict '("\\" "、"))
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation)))

(use-package liberime
  :load-path "~/zenith-emacs/extensions/liberime/build/liberime.so"
  :demand t
  :config
  (liberime-start (expand-file-name "/usr/share/rime-data/")
                  (concat zenith-emacs-root-dir "local/pyim/rime/"))
  (liberime-select-schema "luna_pinyin_simp")
  (setq pyim-default-scheme 'rime))

(provide 'init-pyim)
;;; init-pyim.el ends here
