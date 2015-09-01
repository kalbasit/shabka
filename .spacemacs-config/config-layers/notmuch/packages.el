;;; packages.el --- notmuch Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq notmuch-packages
    '(
      ;; package names go here
      notmuch
      ))

;; List of packages to exclude.
(setq notmuch-excluded-packages '())

;; For each package, define a function notmuch/init-<package-name>
;;
(defun notmuch/init-notmuch ()
  "Initialize my package"
  (use-package notmuch
    :defer t
    :commands notmuch
    :init
    (evil-leader/set-key "om" 'notmuch)
    :config
    (evilify notmuch-hello-mode notmuch-hello-mode-map
             "J" 'notmuch-jump-search)
    (evilify notmuch-search-mode notmuch-search-mode-map
             "J" 'notmuch-jump-search
             "gg" 'notmuch-search-first-thread
             "G" 'notmuch-search-last-thread)
    (evilify notmuch-show-mode notmuch-show-mode-map
             "J" 'notmuch-jump-search)
    (setq notmuch-fcc-dirs '(("cpaulik@gmail.com" . nil)
                             ("christoph.paulik@geo.tuwien.ac.at" . "work-sent")))
    )

  (use-package notmuch
    :bind ("C-c m" . notmuch)
    :init
    :defer t
    :commands notmuch
    :init
    (progn
      (use-package org-notmuch)
      (use-package gnus-alias
        :config
        (progn
          (setq gnus-alias-identity-alist
                '(("personal"
                   nil ;; Does not refer to any other identity
                   "Wael Nasreddine <wael.nasreddine@gmail.com>" ;; Sender address
                   nil ;; No organization header
                   nil ;; No extra headers
                   nil ;; No extra body text
                   "~/.signatures/personal")
                  ("dailymotion"
                   nil
                   "Wael Nasreddine <wmn@dailymotion.com>" ;; Sender address
                   "Dailymotion"
                   nil
                   nil
                   "~/.signatures/dailymotion")
                 ("talentoday"
                   nil
                   "Wael Nasreddine <wmn@talentoday.com>" ;; Sender address
                   "talentoday"
                   nil
                   nil
                   "~/.signatures/talentoday")))
          ;; Define the rules TODO: Add all of personal addresses
          (setq gnus-alias-identity-rules (quote
                                           (("personal" ("any" "wael.nasreddine@gmail.com" both) "personal")
                                            ("dailymotion" ("any" "\\(wmn\\|w.nasreddine\\|wael.nasreddine\\)@dailymotion.com" both) "dailymotion")
                                            ("talentoday" ("any" "\\(wmn\\|w.nasreddine\\|wael.nasreddine\\)@talentoday.com" both) "talentoday"))))
          ;; Use "dailymotion" identity by default
          (setq gnus-alias-default-identity "personal")
          ;; Determine identity when message-mode loads
          (add-hook 'message-setup-hook 'gnus-alias-determine-identity))))
    :config
    (progn
      (setq notmuch-saved-searches
            '((:name "flagged" :query "tag:flagged")
              (:name "deis-new" :query "tag:github::deis AND tag:unread")
              (:name "phabricator-new" :query "tag:unread AND tag:dailymotion::phabricator")
              (:name "jira-new" :query "tag:unread AND tag:dailymotion::jira")
              (:name "inbox-dailymotion-new" :query "tag:dailymotion AND tag:unread AND tag:inbox")
              (:name "dailymotion-jira-new" :query "tag:dailymotion AND tag:unread AND tag:jira")
              (:name "inbox-talentoday-new" :query "tag:talentoday AND tag:unread AND tag:inbox")
              (:name "inbox-personal-new" :query "tag:personal AND tag:unread AND tag:inbox")
              (:name "family-new" :query "tag:family AND tag:unread")
              (:name "wife-new" :query "tag:wife AND tag:unread")
              (:name "consulting-new" :query "tag:consulting AND tag:unread")
              (:name "dailymotion-new" :query "tag:dailymotion AND tag:unread")
              (:name "personal-new" :query "tag:personal AND tag:unread")
              (:name "dailymotion" :query "tag:dailymotion")
              (:name "personal" :query "tag:personal")
              (:name "inbox-unread" :query "tag:inbox AND tag:unread")
              (:name "unread" :query "tag:unread")
              (:name "inbox" :query "tag:inbox")))
      (setq notmuch-address-command
            (f-expand "nottoomuch-addresses" (concat (f-full (getenv "HOME")) "/.bin")))
      (notmuch-address-message-insinuate)
      (setq mail-envelope-from (quote header))
      (setq mail-specify-envelope-from t)
      (setq message-kill-buffer-on-exit t)
      (setq message-sendmail-envelope-from (quote header))
      (setq notmuch-fcc-dirs nil)
      (setq notmuch-search-oldest-first nil)
      (setq send-mail-function (quote sendmail-send-it))
      (setq sendmail-program "/usr/local/bin/msmtp")
      ;; Remove the blue color of flagged messages
      (setq notmuch-search-line-faces (quote
                                        (("flagged" . (:foreground nil)))))
      ;; reply to all by default
      (define-key notmuch-show-mode-map "r" 'notmuch-show-reply)
      (define-key notmuch-show-mode-map "R" 'notmuch-show-reply-sender)
      (define-key notmuch-search-mode-map "r" 'notmuch-search-reply-to-thread)
      (define-key notmuch-search-mode-map "R" 'notmuch-search-reply-to-thread-sender)
      ;; Hide trailing whitespace when showing a message
      (add-hook 'notmuch-show-hook (lambda ()
                                     (setq show-trailing-whitespace nil)))
      ;; Toggle message deletion
      (define-key notmuch-show-mode-map "d"
        (lambda ()
          "toggle deleted tag for message"
          (interactive)
          (if (member "deleted" (notmuch-show-get-tags))
              (notmuch-show-tag (list "-deleted"))
            (notmuch-show-tag (list "+deleted")))))
      ;; Mark as a Spam
      (define-key notmuch-show-mode-map "!"
        (lambda ()
          "toggle spam tag for message"
          (interactive)
          (if (member "spam" (notmuch-show-get-tags))
              (notmuch-show-tag (list "-spam"))
            (notmuch-show-tag (list "+spam")))))
      ;; Kill a thread
      (define-key notmuch-show-mode-map "&"
        (lambda ()
          "toggle killed tag for message"
          (interactive)
          (if (member "killed" (notmuch-show-get-tags))
              (notmuch-show-tag (list "-killed"))
            (notmuch-show-tag (list "+killed")))))
      ;; Bounce a message
      (define-key notmuch-show-mode-map "b"
        (lambda (&optional address)
          "Bounce the current message."
          (interactive "sBounce To: ")
          (notmuch-show-view-raw-message)
          (message-resend address)))
      ;; Archive and mark as read
      (define-key notmuch-search-mode-map "A"
        (lambda (&optional beg end)
          "archive and mark as read"
          (interactive (notmuch-search-interactive-region))
          (notmuch-search-tag (list "-unread" "-inbox") beg end)
          (when (eq beg end)
            (notmuch-search-next-thread))))
      ;; View inline patch as diff
      (defun my-notmuch-show-view-as-patch ()
        "View the the current message as a patch."
        (interactive)
        (let* ((id (notmuch-show-get-message-id))
               (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
               (diff-default-read-only t)
               (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
               (map (make-sparse-keymap)))
          (define-key map "q" 'notmuch-kill-this-buffer)
          (switch-to-buffer buf)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert subject)
            (insert (notmuch-get-bodypart-internal id 1 nil)))
          (set-buffer-modified-p nil)
          (diff-mode)
          (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
            (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
          (goto-char (point-min))))
      (define-key 'notmuch-show-mode-map "D" 'my-notmuch-show-view-as-patch)
      ;; At startup position the cursor on the first saved searches
      (add-hook 'notmuch-hello-refresh-hook
                (lambda ()
                  (if (and (eq (point) (point-min))
                           (search-forward "Saved searches:" nil t))
                      (progn
                        (forward-line)
                        (widget-forward 1))
                    (if (eq (widget-type (widget-at)) 'editable-field)
                        (beginning-of-line)))))))


  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
