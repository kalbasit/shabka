(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'org-mu4e)

(setq mu4e-view-show-images t)
;; use imagemagick if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
(setq mu4e-html2text-command "html2text -utf8 -width 80")
;; default
;; (setq mu4e-maildir "~/Maildir")

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( ("/INBOX"               . ?i)
       ("/[Gmail].Sent Mail"   . ?s)
       ("/[Gmail].Trash"       . ?t)
       ("/[Gmail].All Mail"    . ?a)))

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")

;; something about ourselves
(setq
   user-mail-address "cpaulik@gmail.com"
   user-full-name  "Christoph Paulik"
   mu4e-compose-signature
    (concat
      ""
      "")
    mu4e-user-mail-address-list
    '("cpaulik@gmail.com"
      "christoph.paulik@geo.tuwien.ac.at"
      "Christoph.Paulik@geo.tuwien.ac.at")
    )

;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "cpaulik@gmail.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

;; alternatively, for emacs-24 you can use:
;;(setq message-send-mail-function 'smtpmail-send-it
;;     smtpmail-stream-type 'starttls
;;     smtpmail-default-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-server "smtp.gmail.com"
;;     smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
(setq mu4e-use-fancy-chars t
        mu4e-headers-draft-mark     '("D" . "⚒ ")  ; draft
        mu4e-headers-seen-mark      '("S" . "☑ ")  ; seen
        mu4e-headers-unseen-mark    '("u" . "☐ ")  ; unseen
        mu4e-headers-flagged-mark   '("F" .  "⚵ ") ; flagged
        mu4e-headers-new-mark       '("N" .  "✉ ") ; new
        mu4e-headers-replied-mark   '("R" . "↵ ")  ; replied
        mu4e-headers-passed-mark    '("P" . "⇉ ")  ; passed
        mu4e-headers-encrypted-mark '("x" . "⚷ ")  ; encrypted
        mu4e-headers-signed-mark    '("s" . "✍ ")) ; signed

;;; message view action
(defun mu4e-msgv-action-view-in-browser (msg)
      "View the body of the message in a web browser."
      (interactive)
      (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
            (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
        (unless html (error "No html part for this message"))
        (with-temp-file tmpfile
          (insert
           "<html>"
           "<head><meta http-equiv=\"content-type\""
           "content=\"text/html;charset=UTF-8\">"
           html))
        (browse-url (concat "file://" tmpfile))))

    (add-to-list 'mu4e-view-actions
                 '("View in browser" . mu4e-msgv-action-view-in-browser) t)
(setq
   mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
   mu4e-update-interval 300)             ;; update every 5 minutes

(add-hook 'mu4e-view-mode-hook 'visual-line-mode)

;; send mail from address that received it
(add-hook 'mu4e-compose-pre-hook
          (defun my-set-from-address ()
            "Set the From address based on the To address of the original."
            (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
              (if msg
                  (setq user-mail-address
                        (cond
                         ((mu4e-message-contact-field-matches msg :to "@gmail")
                          "cpaulik@gmail.com")
                         ((mu4e-message-contact-field-matches msg :to "Christoph.Paulik@geo.tuwien.ac.at")
                          "Christoph.Paulik@geo.tuwien.ac.at")
                         ((mu4e-message-contact-field-matches msg :to "cpa@ipf.tuwien.ac.at")
                          "Christoph.Paulik@geo.tuwien.ac.at")
                         (t "cpaulik@gmail.com")))))))

;; not possible because message body is not available in headers view
(add-to-list 'mu4e-header-info-custom
  '(:abstract .
    ( :name "first n characters of the message"  ;; long name, as seen in the message-view
      :shortname "Message"           ;; short name, as seen in the headers view
      :help "Number of recipients for this message" ;; tooltip
      :function
      (lambda (msg)
	(subseq (mu4e-message-field msg :body-txt) 0 5)
	  ))))

;;remove list from headers view
(setq mu4e-headers-fields (delete '(:mailing-list . 10) mu4e-headers-fields))
;;(add-to-list 'mu4e-headers-fields '(:abstract . 50))

(setq mu4e-hide-index-messages t)

;; add new bookmarks
(add-to-list 'mu4e-bookmarks
	     '("flag:flagged" "Flagged/Starred messages" ?f))

(provide 'mu4e_config)
