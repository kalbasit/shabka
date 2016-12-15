;; Python indent with two spaces
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq tab-width 2))))
