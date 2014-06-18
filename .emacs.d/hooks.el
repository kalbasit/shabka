;; Python indent with two spaces
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq tab-width 2))))

;; Enable flyspell for text mode
(add-hook 'text-mode-hook 'enable-flyspell)
