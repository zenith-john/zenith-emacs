;;; init-snails.el --- ivy-snails

;;; Commentary:
;;;

;;; Code:

;; snails
(use-package snails
  :config
  (require 'snails)

  (defun zenith/enable-truncate-lines ()
    "Enable truncate lines locally for snails-tip buffer"
    (with-current-buffer (get-buffer-create snails-tips-buffer)
      (setq-local truncate-lines nil)))
  (add-hook 'snails-mode-hook 'zenith/enable-truncate-lines)

  ;; Use arrow to move and <escape> to quit.
  (general-define-key
   :keymaps 'snails-mode-map
   "<down>" 'snails-select-next-item
   "<up>" 'snails-select-prev-item
   "<escape>" 'snails-quit)

  ;; I don't know why, but the ivy-mode set <escape> to minibuffer-keyboard-quit
  ;; and overwrite the keybindings in snails-mode-map.
  (general-define-key
   :keymaps 'ivy-mode-map
   [escape] nil)

  ;; If the project contains too much files, the snails will freeze, so limit the
  ;; file number to 50 in search result.
  (snails-create-sync-backend
   :name
   "PROJECTILEZ"

   :candidate-filter
   (lambda (input)
     (let ((candidates)
           (project-files (snails-backend-projectile-candidates))
           (count 0))
       (when project-files
         (dolist (file project-files)
           (when (and (< count 50)
                      (or
                       (string-equal input "")
                       (snails-match-input-p input file)))
             (snails-add-candiate 'candidates (snails-wrap-file-icon file) file)
             (setq count (+ count 1)))))
       (snails-sort-candidates input candidates 1 1)
       candidates))

   :candiate-do
   (lambda (candidate)
     (let ((project-root (snails-backend-projectile-project-root)))
       (find-file (expand-file-name candidate project-root)))))

  ;; Don not show misc buffer in snails
  (setq snails-backend-buffer-blacklist
        (append snails-backend-buffer-blacklist '(" *company-box"
                                                  " *string-output"
                                                  " *which-key"
                                                  "*helpful"
                                                  "*snails tips"
                                                  "*Help"
                                                  " *git-gutter"
                                                  " *Ibuffer"
                                                  " *"
                                                  "*ccls"
                                                  "*lsp")))

  (setq snails-default-backends '(snails-backend-buffer snails-backend-bookmark snails-backend-recentf snails-backend-directory-files))
  (setq snails-prefix-backends
        '((">"
           '(snails-backend-command))
          ("@"
           '(snails-backend-imenu))
          ("#"
           '(snails-backend-current-buffer))
          ("!"
           '(snails-backend-rg))
          ("?"
           '(snails-backend-projectilez snails-backend-fd)))))

(provide 'init-snails)
