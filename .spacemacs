;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

;; Configuration Layers
;; --------------------

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs-config/config-layers/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '(ansible
                                       (auto-completion
                                        :variables
                                        auto-completion-enable-help-tooltip t)
                                       chrome
                                       colors
                                       custom_org_config
                                       emacs-lisp
                                       emoji
                                       erc
                                       eyebrowse
                                       games
                                       (git
                                        :variables
                                        git-gutter-use-fringe t)
                                       github
                                       gnus
                                       html
                                       ipython-notebook
                                       lua
                                       markdown
                                       my_python
                                       notmuch
                                       org
                                       pandoc
                                       ;; php
                                       (python
                                        :variables
                                        python-enable-yapf-format-on-save nil
                                        python-test-runner 'pytest)
                                       racket
                                       ranger
                                       research-config
                                       search-engine
                                       semantic
                                       (shell
                                        :variables
                                        shell-default-shell 'term
                                        shell-default-term-shell "/bin/zsh")
                                       syntax-checking
                                       themes-megapack
                                       typing-games
                                       xkcd)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(material-theme evil-escape)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '(magit-annex)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

;; Initialization Hooks
;; --------------------

(defun dotspacemacs/init ()
  "User initialization for Spacemacs. This function is called at the very
 startup."
    (setq-default
     use-package-verbose t
    ;; Specify the startup banner. If the value is an integer then the
    ;; banner with the corresponding index is used, if the value is `random'
    ;; then the banner is chosen randomly among the available banners, if
    ;; the value is nil then no banner is displayed.
    dotspacemacs-startup-banner 'official
    ;; List of themes, the first of the list is loaded when spacemacs starts.
    ;; Press <SPC> T n to cycle to the next theme in the list (works great
    ;; with 2 themes variants, one dark and one light)
    dotspacemacs-themes '(material material-light)
    ;; The leader key
    dotspacemacs-leader-key "SPC"
    ;; Major mode leader key is a shortcut key which is the equivalent of
    ;; pressing `<leader> m`
    dotspacemacs-major-mode-leader-key ","
    ;; The command key used for Evil commands (ex-commands) and
    ;; Emacs commands (M-x).
    ;; By default the command key is `:' so ex-commands are executed like in Vim
    ;; with `:' and Emacs commands are executed with `<leader> :'.
    dotspacemacs-command-key ":"
    ;; which-key delay in seconds. The which-key is the popup buffer listing
    ;; the commands bound to the current keystrokes.
    which-key-idle-delay 0.4
    ;; If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only).
    dotspacemacs-fullscreen-at-startup nil
    ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
    ;; Use to disable fullscreen animations in OSX."
    dotspacemacs-fullscreen-use-non-native nil
    ;; If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
    ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
    dotspacemacs-maximized-at-startup nil
    ;; A value from the range (0..100), in increasing opacity, which describes the
    ;; transparency level of a frame when it's active or selected. Transparency can
    ;; be toggled through `toggle-transparency'.
    dotspacemacs-active-transparency 90
    ;; A value from the range (0..100), in increasing opacity, which describes the
    ;; transparency level of a frame when it's inactive or deselected. Transparency
    ;; can be toggled through `toggle-transparency'.
    dotspacemacs-inactive-transparency 90
    ;; If non nil unicode symbols are displayed in the mode line (e.g. for lighters)
    dotspacemacs-mode-line-unicode-symbols t
    ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth scrolling
    ;; overrides the default behavior of Emacs which recenters the point when
    ;; it reaches the top or bottom of the screen
    dotspacemacs-smooth-scrolling t
    ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
    dotspacemacs-smartparens-strict-mode nil
    ;; If non nil advises quit functions to keep server open when quitting.
    dotspacemacs-persistent-server nil
    ;; The default package repository used if no explicit repository has been
    ;; specified with an installed package.
    ;; Not used for now.
    dotspacemacs-default-package-repository nil
    ;; set preview in latex files bigger
    preview-scale-function 1.6
    dotspacemacs-mode-line-unicode-symbols nil)

    (if window-system (setq dotspacemacs-mode-line-unicode-symbols t))
    ;; set the default font
    (when (string= system-name "cpa")
    ;; work virtual machine
    (setq dotspacemacs-default-font '("Source Code Pro"
                                    :size 13
                                            :weight normal
                                            :width normal
                                            :powerline-scale 1.3))
    )
    (when (string= system-name "cp-Lenovo-Yoga-2-Pro")
    ;; home laptop with hdpi screen
    (setq dotspacemacs-default-font '("Source Code Pro"
                                    :size 22
                                            :weight normal
                                            :width normal
                                            :powerline-scale 1.3))
  )
  (add-to-list 'load-path "~/.spacemacs-config/org-mode/lisp")
  (add-to-list 'load-path "~/.spacemacs-config/org-mode/contrib/lisp" t)
  (add-to-list 'load-path "~/.spacemacs-config" t)
  (add-to-list 'load-path "~/.spacemacs-config/org-reveal" t)
  (add-to-list 'load-path "~/.spacemacs-config/org-ref" t)
  (add-to-list 'load-path "~/.spacemacs-config/material-theme" t)

  (defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

    (when window-system (set-exec-path-from-shell-PATH))

    ;; Get email, and store in nnml
    (setq gnus-secondary-select-methods '(
                                        (nntp "gmane"
                                                (nntp-address "news.gmane.org"))
                                        (nntp "news.eternal-september.org")
                                        (nntp "nntp.aioe.org")
                                        (nntp "news.gwene.org")
                                        ))
    ; Send email via Gmail:
    (setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com")
    ; Archive outgoing email in Sent folder on imap.gmail.com:
    (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail")
    (setq gnus-posting-styles
        '(((header "to" "cpaulik@gmail.com")
           (address "cpaulik@gmail.com"))
    ((header "to" "christoph.paulik@geo.tuwien.ac.at")
         (address "christoph.paulik@geo.tuwien.ac.at"))))
    (setq nnml-directory "~/gmail")
    (setq message-directory "~/gmail")
  )

(defun dotspacemacs/config ()
  "This is were you can ultimately override default Spacemacs configuration.
This function is called at the very end of Spacemacs initialization."
  (use-package helm
    :defer 5
    :commands (helm-execute-persistent-action
               helm-select-action
               helm-grep-mode-jump-other-window
               helm-grep-mode-jump-other-window-forward
               helm-grep-mode-jump-other-window-backward)
    :config
    (progn
        (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
        (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
        (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
        (define-key helm-map (kbd "C-S-j")  'helm-scroll-other-window)
        (define-key helm-map (kbd "C-S-k")  'helm-scroll-other-window-down)
        (define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
        (define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
        (define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)
        (setq helm-split-window-in-side-p t)
      )
    )
    ;; set terminal command
    (defun start-term () (interactive) (term "/bin/zsh"))
    (evil-leader/set-key "at" 'start-term)

    (setq flycheck-check-syntax-automatically '(save mode-enabled idle-change))
    (setq flycheck-idle-change-delay 5)
    (require 'org-pelican)
    ; Set name and email:
    (when (string= system-name "cpa")
      ;; work virtual machine
      (setq user-full-name "Christoph Paulik"
            user-mail-address "christoph.paulik@geo.tuwien.ac.at")
      )
    (when (string= system-name "cp-Lenovo-Yoga-2-Pro")
      ;; home laptop
      (setq user-full-name "Christoph Paulik"
            user-mail-address "cpaulik@gmail.com")
      )

    (use-package erc
      :defer t
      :commands connect-erc
      :init (evil-leader/set-key "oe" 'connect-erc)
      :config
      (progn
        (setq erc-autojoin-channels-alist
              '(("0.0" "#syl20bnr/spacemacs")))
        (defun connect-erc ()
          (interactive)
          (erc-tls :server "irc.gitter.im" :nick "cpaulik" :password nil)
        )
      ))

    (global-set-key (kbd "<C-mouse-4>") 'zoom-frm-in)
    (global-set-key (kbd "<C-mouse-5>") 'zoom-frm-out)
    (global-set-key (kbd "C-+") 'zoom-frm-in)
    (global-set-key (kbd "C--") 'zoom-frm-out)
    (setq dired-listing-switches "-alhk")
    (setq dired-listings-switches "-alhk")
    (setq wdired-allow-to-change-permissions t)
    (setq compilation-finish-function nil)
    ;; Make evil-mode up/down operate in screen lines instead of logical lines
    (define-key evil-normal-state-map "j" 'evil-next-visual-line)
    (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
    ;; Also in visual mode
    (define-key evil-visual-state-map "j" 'evil-next-visual-line)
    (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
    (setq doc-view-resolution 300)
    (setq vc-follow-symlinks t)
    ;; set default browser
    (setq browse-url-generic-program "google-chrome")
    (setq browse-url-browser-function 'browse-url-generic)
    ;; set python company backends
    (setq company-backends-python-mode '((company-anaconda :with company-dabbrev-code :with company-yasnippet)))
    (setq company-transformers '(spacemacs//company-transformer-cancel
                                 company-sort-by-backend-importance))
    ;; open file in dired with standard program
    (defun dired-open-file ()
      "In dired, open the file named on this line."
      (interactive)
      (let* ((file (dired-get-filename nil t)))
        (message "Opening %s..." file)
        (call-process "xdg-open" nil 0 nil file)
        (message "Opening %s done" file)))


)


;; Custom variables
;; ----------c------

;; Do not write anything in this section. This is where Emacs will
;; auto-generate custom variable definitions.


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4 t)
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(custom-safe-themes
   (quote
    ("5034b2b1b21f14c4c6223d7255f6498ced1168f7183a094d24c6fa7a5083886e" default)))
 '(paradox-github-token t)
 '(ring-bell-function (quote ignore) t)
 '(safe-local-variable-values (quote ((hl-sexp-mode) (rainbow-mode . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
