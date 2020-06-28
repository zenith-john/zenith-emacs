;;; init-pyim.el --- pyim configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:

;; pyim
(require 'pyim)

;; liberime
(setq liberime-shared-data-dir "/usr/share/rime-data"
      liberime-user-data-dir (concat zenith-emacs-local-dir "pyim/rime"))

(with-eval-after-load 'pyim
  (require 'liberime nil t)
  (liberime-start liberime-shared-data-dir liberime-user-data-dir)
  (liberime-select-schema "luna_pinyin_simp")
  (add-to-list 'pyim-punctuation-dict '("\\" "、"))
  ;; (liberime-sync)
  ;; The sync do not work, do manual sync
  (setq pyim-default-scheme 'rime-quanpin)
  (pyim-isearch-mode 1))

(setq pyim-dcache-directory (concat zenith-emacs-local-dir "pyim/dcache/")
      pyim-page-tooltip 'posframe
      default-input-method "pyim"
      pyim-liberime-search-limit 100)

(setq-default pyim-english-input-switch-functions
              '(pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode))

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))

;; temporarily using emacs-rime as liberime not usable yet in emacs27
;; (require 'rime)

;; (setq
;;  rime-user-data-dir "~/.config/fcitx/rime"
;;  default-input-method "rime"
;;  rime-show-candidate 'posframe)

(provide 'init-pyim)
;;; init-pyim.el ends here
