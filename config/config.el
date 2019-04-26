;;; config.el -*- lexical-binding: t; -*-
;;; Code:
(use-package keyfreq
  :commands (keyfreq-mode keyfreq-show keyfreq-reset)
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands '(org-self-insert-command
                                    self-insert-command
                                    next-line
                                    previous-line
                                    right-char
                                    left-char
                                    cua-scroll-down
                                    company-ignore
                                    org-delete-backward-char
                                    python-indent-dedent-line
                                    python-indent-dedent-line-backspace
                                    delete-backward-char
                                    org-return
                                    ivy-next-line
                                    ivy-backward-delete-char
                                    company-select-next-or-abort
                                    company-select-previous-or-abort
                                    end-of-line
                                    magit-next-line
                                    mwheel-scroll
                                    isearch-printing-char
                                    newline
                                    mouse-drag-region
                                    org-cycle
                                    ivy-previous-line
                                    org-meta-return
                                    mouse-set-point
                                    kill-line
                                    find-file
                                    org-agenda-next-line
                                    ivy-done
                                    minibuffer-keyboard-quit
                                    magit-previous-line
                                    beginning-of-line
                                    indent-for-tab-command
                                    evil-previous-line
                                    evil-next-line
                                    backward-delete-char-untabify
                                    evil-forward-char
                                    exit-minibuffer
                                    evil-backward-char
                                    exit-minibuffer
                                    evil-ex
                                    evil-normal-state
                                    )))

(use-package ox-hugo
  :after ox
  :init
  (require 'ox-hugo))

(use-package org-noter
  :commands (org-noter)
  :after pdf-tools
  :config
  ;; Overriding the function due to make visual-line mode no effect.
  (defun org-noter--set-notes-scroll (window &rest ignored)
    nil)
  (setq org-noter-always-create-frame nil
        org-noter-kill-frame-at-session-end nil
        org-noter-hide-other nil)

  (map! :map pdf-view-mode-map :gn "C-i" #'org-noter-insert-note-toggle-no-questions)
  (map! :map pdf-view-mode-map :gn "q" #'org-noter-kill-session)
  (map! :map pdf-view-mode-map :gn "i" #'org-noter-insert-note))

(use-package fd-dired
  :commands (fd-dired)
  :after (evil)
  :init
  (when (executable-find "fd")
    (evil-define-command +evil:fd (query)
      "Ex interface for fd-dired"
      (interactive "<a>")
      (fd-dired (file-name-directory (buffer-file-name)) query))
    (evil-ex-define-cmd "fd" #'+evil:fd)))

(use-package sdcv
  :commands (sdcv-search-input+ sdcv-search-pointer+)
  :init
  (map! :ni "C-;" #'sdcv-search-pointer+)
  :config
  (setq sdcv-say-word-p t)

  (setq sdcv-dictionary-data-dir "/usr/share/stardict/dic/")

  (setq sdcv-dictionary-simple-list '("简明英汉字典增强版")))

(use-package company-english-helper
  :commands (toggle-company-english-helper))

(use-package insert-translated-name
  :commands (insert-translated-name-insert)
  :init
  (map! :i "C-'" #'insert-translated-name-insert)
  (defun +zenith/advice-insert-translated-name-active (style)
    (interactive "P")
    (add-hook 'after-change-functions 'insert-translated-name-monitor-after-change t t))
  (advice-add! 'insert-translated-name-active :before #'+zenith/advice-insert-translated-name-active))


(use-package auto-save
  :init
  (require 'auto-save)

  (setq auto-save-idle 2
        auto-save-silent t
        auto-save-delete-trailing-whitespace t)

  (auto-save-enable))

(use-package awesome-tab
  :commands (awesome-tab-build-ivy-source awesome-tab-select-visible-tab awesome-tab-mode)
  :init
  (setq awesometab-hide-tabs-hooks
        '(magit-status-mode-hook magit-popup-mode-hook reb-mode-hook helpful-mode-hook))
  (setq awesome-tab-style 'chamfer
        awesome-tab-label-fixed-length 14)
  (awesome-tab-mode t)
  (map! :leader
        :nv
        "TAB" #'awesome-tab-build-ivy-source)
  (dotimes (i 10)
    (map! :nvi (concat "M-" (int-to-string i)) #'awesome-tab-select-visible-tab)))

(use-package pyim
  :init
  (setq pyim-page-tooltip 'posframe)
  (setq default-input-method "pyim")

  (map! "M-/" 'pyim-convert-string-at-point)
  (use-package liberime
    :load-path "~/zenith-emacs/extensions/liberime/build/liberime.so"
    :config
    (liberime-start (expand-file-name "/usr/share/rime-data/")
                    (concat zenith-emacs-root-dir "local/pyim/rime/"))
    (liberime-select-schema "luna_pinyin_simp")
    (setq pyim-default-scheme 'rime))
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

(use-package org-edit-latex
  :after org
  (require 'org-edit-latex))

(use-package org-ref
  :after org
  :init
  (require 'bibtex-completion)
  (setq org-ref-completion-library 'org-ref-reftex)
  ;; Copy from org-ref-ivy-cite
  (defhydra org-ref-cite-hydra (:color blue)
    "
_p_: Open pdf     _w_: WOS          _g_: Google Scholar _K_: Copy citation to clipboard
_u_: Open url     _r_: WOS related  _P_: Pubmed         _k_: Copy key to clipboard
_n_: Open notes   _c_: WOS citing   _C_: Crossref       _f_: Copy formatted entry
_o_: Open entry   _e_: Email entry  ^ ^                 _q_: quit
"
    ("o" org-ref-open-citation-at-point nil)
    ("p" org-ref-open-pdf-at-point nil)
    ("n" org-ref-open-notes-at-point nil)
    ("u" org-ref-open-url-at-point nil)
    ("w" org-ref-wos-at-point nil)
    ("r" org-ref-wos-related-at-point nil)
    ("c" org-ref-wos-citing-at-point nil)
    ("g" org-ref-google-scholar-at-point nil)
    ("P" org-ref-pubmed-at-point nil)
    ("C" org-ref-crossref-at-point nil)
    ("K" org-ref-copy-entry-as-summary nil)
    ("k" (progn
           (kill-new
            (car (org-ref-get-bibtex-key-and-file))))
     nil)
    ("f" (kill-new
          (org-ref-format-entry (org-ref-get-bibtex-key-under-cursor)))
     nil)

    ("e" (kill-new (save-excursion
                     (org-ref-open-citation-at-point)
                     (org-ref-email-bibtex-entry)))
     nil)
    ("q" nil))
  :config
  (setq org-ref-cite-onclick-function (lambda (_) (org-ref-cite-hydra/body)))
  (setq org-ref-pdf-directory "~/Documents/Library/")
  (setq org-ref-default-bibliography '("~/Dropbox/Library.bib"))

  ;; Make citation work
  (setq org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "biber %b"
          "%latex -interaction nonstopmode -output-directory %o %f"
          "%latex -interaction nonstopmode -output-directory %o %f")))

(use-package lsp-python-ms
  :init
  (setq lsp-python-ms-dir
        (concat zenith-emacs-extension-dir "python-language-server/output/bin/Release/"))
  (add-hook! python-mode (lsp)))

;;; config.el ends here
