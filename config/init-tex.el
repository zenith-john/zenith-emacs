;;; init-tex.el --- tex configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
;; Latex support
(with-eval-after-load 'tex
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-bibpath-environment-variables '("/home/zenith-john/Dropbox/"))
  (setq reftex-bibliography-commands '("bibliography" "nobibiliography" "addbibresource"))
  (setq-default TeX-engine 'xetex
                TeX-show-compilation t)

  (add-to-list 'TeX-command-list
               '("XeLaTeX" "xelatex -interaction=nonstopmode %s"
                 TeX-run-command t t :help "Run xelatex") t))

(provide 'init-tex)
;;; init-tex.el ends here
