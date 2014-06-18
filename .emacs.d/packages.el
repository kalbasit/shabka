(use-package company
  :init (global-company-mode 1)
  :config
  (progn
    (setq company-idle-delay 0.5)
    (setq company-tooltip-limit 10)
    (setq company-minimum-prefix-length 2)

    ;; invert the navigation direction if the the completion popup-isearch-match
    ;; is displayed on top (happens near the bottom of windows)
    (setq company-tooltip-flip-when-above t)))

(use-package notmuch
  :commands 'notmuch
  :bind ("C-c m" . notmuch)
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
          ("work"
           nil
           "Wael Nasreddine <wmn@google.com>" ;; Sender address
           "Google"
           nil
           nil
           "~/.signatures/work")))
        ;; Define the rules TODO: Add all of personal addresses
        (setq gnus-alias-identity-rules (quote
                                          (("personal" ("any" "wael.nasreddine@gmail.com" both) "personal")
                                           ("work" ("any" "\\(wmn\\|wnasreddine\\)@google.com" both) "work"))))
        ;; Use "work" identity by default
        (setq gnus-alias-default-identity "work")
        ;; Determine identity when message-mode loads
        (add-hook 'message-setup-hook 'gnus-alias-determine-identity))))
  :config
  (progn
    (setq notmuch-saved-searches
      '((:name "flagged" :query "tag:flagged")
        (:name "tome-work" :query "tag:unread AND tag:work AND to:wmn@google.com")
        (:name "family-new" :query "tag:family AND tag:unread")
        (:name "wife-new" :query "tag:wife AND tag:unread")
        (:name "consulting-new" :query "tag:consulting AND tag:unread")
        (:name "inbox-work-new" :query "tag:work AND tag:unread AND tag:inbox")
        (:name "inbox-personal-new" :query "tag:personal AND tag:unread AND tag:inbox")
        (:name "work-new" :query "tag:work AND tag:unread")
        (:name "personal-new" :query "tag:personal AND tag:unread")
        (:name "work" :query "tag:work")
        (:name "personal" :query "tag:personal")
        (:name "inbox-unread" :query "tag:inbox AND tag:unread")
        (:name "unread" :query "tag:unread")
        (:name "inbox" :query "tag:inbox")))
    (setq notmuch-address-command
      (f-expand "nottoomuch-addresses" (concat (f-full (getenv "HOME")) "/.filesystem/bin")))
    (notmuch-address-message-insinuate)
    (setq mail-envelope-from (quote header))
    (setq mail-specify-envelope-from t)
    (setq message-kill-buffer-on-exit t)
    (setq message-sendmail-envelope-from (quote header))
    (setq notmuch-fcc-dirs nil)
    (setq notmuch-search-oldest-first nil)
    (setq send-mail-function (quote sendmail-send-it))
    (setq sendmail-program "/usr/bin/msmtp")
    ;; reply to all by default
    (define-key notmuch-show-mode-map "r" 'notmuch-show-reply)
    (define-key notmuch-show-mode-map "R" 'notmuch-show-reply-sender)
    (define-key notmuch-search-mode-map "r" 'notmuch-search-reply-to-thread)
    (define-key notmuch-search-mode-map "R" 'notmuch-search-reply-to-thread-sender)
    ;; Remove the blue color of flagged messages
    (setq notmuch-search-line-faces '((\"flagged\" . (:foreground nil))))
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

(use-package org
  :bind(("C-c l" . org-store-link)
  ("C-c c" . org-capture)
  ("C-c a" . org-agenda)
  ("C-c b" . org-iswitchb)))

(use-package hl-line
  :config (set-face-background 'hl-line "#073642"))

(use-package ido
  :init (ido-mode 1)
  :config
  (progn
    (setq ido-case-fold t)
    (setq ido-everywhere t)
    (setq ido-enable-prefix nil)
    (setq ido-enable-flex-matching t)
    (setq ido-create-new-buffer 'always)
    (setq ido-use-filename-at-point 'guess)
    (setq ido-save-directory-list-file (f-expand "ido.hst" emacs-cache-dir))
    (setq ido-max-prospects 10)
    (setq ido-file-extensions-order '(".py" ".rb" ".el" ".coffee" ".js"))
    (add-to-list 'ido-ignore-files "\\.DS_Store")))

(use-package calc
  :commands calc)

(use-package flx-ido
  :init (flx-ido-mode 1)
  :config (setq ido-use-face nil))

(use-package ido-vertical-mode
  :init (ido-vertical-mode 1))

(use-package smex
  :init (smex-initialize)
  :bind (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)))

(use-package ido-ubiquitous
  :init (ido-ubiquitous-mode +1))

(use-package prodigy
  :init (progn
    (add-hook 'prodigy-mode-hook
        (lambda ()
          (setq-local show-trailing-whitespace nil))))
  :demand t
  :bind ("C-x p" . prodigy))

(use-package ibuffer
  :config (setq ibuffer-expert t)
  :bind ("C-x C-b" . ibuffer))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (bind-key "C-c C-a" 'magit-just-amend magit-mode-map))
  :config
  (progn
    (setq magit-default-tracking-name-function 'magit-default-tracking-name-branch-only)
    (setq magit-set-upstream-on-push t)
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (setq magit-stage-all-confirm nil)
    (setq magit-unstage-all-confirm nil)
    (setq magit-restore-window-configuration t)
    (add-hook 'magit-mode-hook 'rinari-launch))
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :init (global-git-gutter-mode +1))

(use-package ack-and-a-half
  :commands (ack-and-a-half ack-and-a-half-same ack-and-a-half-find-file ack-and-a-half-find-file-same)
  :init
  (progn
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)))

(use-package swoop
  :commands swoop
  :bind ("C-o" . swoop))

(use-package ag
  :commands ag)

(use-package projectile
  :init (projectile-global-mode 1)
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'ido)
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package web-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))
  :config
  (progn
    (add-hook 'web-mode-hook
        (lambda ()
    (setq web-mode-style-padding 2)
    (setq web-mode-script-padding 2)))))

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (use-package smartparens-latex)
    (show-smartparens-global-mode 1))
  :config
  (progn
    (setq smartparens-strict-mode t)
    (setq sp-autoescape-string-quote nil)
    (setq sp-autoinsert-if-followed-by-word t)
    (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))))

(use-package ruby-mode
  :init
  (progn
    (use-package ruby-tools)
    (use-package rhtml-mode
      :mode (("\\.rhtml$" . rhtml-mode)
       ("\\.html\\.erb$" . rhtml-mode)))
    (use-package rinari
      :init (global-rinari-mode 1)
      :config (setq ruby-insert-encoding-magic-comment nil))
    (use-package rspec-mode
      :config
      (progn
        (setq rspec-use-rake-flag nil)
        (defadvice rspec-compile (around rspec-compile-around activate)
          "Use BASH shell for running the specs because of ZSH issues."
          (let ((shell-file-name "/bin/bash"))
            ad-do-it)))))
  :config
  (progn
    (setq ruby-deep-indent-paren nil))
  :bind (("C-M-h" . backward-kill-word)
   ("C-M-n" . scroll-up-five)
   ("C-M-p" . scroll-down-five))
  :mode (("\\.rake$" . ruby-mode)
   ("\\.gemspec$" . ruby-mode)
   ("\\.ru$" . ruby-mode)
   ("Rakefile$" . ruby-mode)
   ("Gemfile$" . ruby-mode)
   ("Capfile$" . ruby-mode)
   ("Guardfile$" . ruby-mode)))

(use-package markdown-mode
  :config
  (progn
    (bind-key "M-n" 'open-line-below markdown-mode-map)
    (bind-key "M-p" 'open-line-above markdown-mode-map))
  :mode (("\\.markdown$" . markdown-mode)
   ("\\.md$" . markdown-mode)))

(use-package erc
  :commands erc
  :bind ("C-c C-e" . erc-start-or-switch)
  :config
  (progn
    (setq erc-modules
          (quote
            (autoaway autojoin button completion fill irccontrols
             list match menu move-to-prompt netsplit networks
             noncommands notify readonly ring scrolltobottom smiley
             stamp spelling track truncate unmorse)))
    (load "~/.ercpass") ;; load erc passwords
    (setq erc-autojoin-mode t)
    (setq erc-autojoin-channels-alist
    '((".*\\.freenode.net" "#notmuch" "#emacs")
      (".*\\.twice-irc.de" "#i3")
      (".*irc.*\\.corp.google.com" "#ci" "#ci-oncall" "#corpdb" "#goobuntu")))
    ;; don't show any of this
    (setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
    ;; don't prompt for nickserv password
    (setq erc-prompt-for-nickserv-password nil)))

(use-package drag-stuff
  :init (drag-stuff-global-mode 1)
  :bind (("M-N" . drag-stuff-down)
   ("M-P" . drag-stuff-up)))

(use-package flyspell
  :commands flyspell-mode
  :config
  (progn
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra"))))

(use-package go-mode
  :mode ("\\.go$" . go-mode)
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)
    (bind-key "C-c C-r" 'go-remove-unused-imports go-mode-map)
    (bind-key "C-c i" 'go-goto-imports go-mode-map)))

(use-package flycheck
  :init (global-flycheck-mode +1)
  :config (add-hook 'prog-mode-hook 'flycheck-mode))

(use-package anzu
  :init (global-anzu-mode))

(use-package undo-tree
  :init (global-undo-tree-mode))
