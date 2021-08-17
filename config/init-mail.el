;;; init-mail.el ---  configure mu4e for emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;;

;;; Code:
(setq mail-user-agent 'mu4e-user-agent
      user-full-name "Zenith John"
      message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it)

(setq mu4e-get-mail-command "offlineimap"
      mu4e-update-interval 600
      mu4e-maildir-list "~/Documents/Mail"
      mu4e-view-use-old nil
      mu4e-sent-messages-behavior 'delete)

(setq mu4e-sent-folder "/Foxmail/Sent Messages"
      mu4e-drafts-folder "/Foxmail/Drafts"
      user-mail-address "zenith-john@foxmail.com"
      smtpmail-smtp-server "smtp.qq.com"
      smtpmail-local-domain "qq.com"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl)

(defvar zenith/mu4e-account-alist
  '(("Foxmail"
     (mu4e-sent-folder "/Foxmail/Sent Messages")
     (mu4e-drafts-folder "/Foxmail/Drafts")
     (user-mail-address "zenith-john@foxmail.com")
     (smtpmail-smtp-server "smtp.qq.com")
     (smtpmail-local-domain "qq.com")
     (smtpmail-smtp-service 465)
     (smtpmail-stream-type ssl))
    ("Tsinghua"
     (mu4e-sent-folder "/Tsinghua/Sent Items")
     (mu4e-drafts-folder "/Tsinghua/Drafts")
     (user-mail-address "znt21@mails.tsinghua.edu.cn")
     (smtpmail-smtp-server "mails.tsinghua.edu.cn")
     (smtpmail-local-domain "mails.tsinghua.edu.cn")
     (smtpmail-smtp-service 465)
     (smtpmail-stream-type ssl))))

(defun zenith/mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                zenith/mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) zenith/mu4e-account-alist)
                             nil t nil nil (caar zenith/mu4e-account-alist))))
         (account-vars (cdr (assoc account zenith/mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'zenith/mu4e-set-account)

(zenith/autoload '(mu4e) "mu4e")

(with-eval-after-load 'mu4e
  (require 'smtpmail)
  (require 'mu4e)
  (require 'mu4e-alert)
  (mu4e-alert-enable-notifications))

(provide 'init-mail)
