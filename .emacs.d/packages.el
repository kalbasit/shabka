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
