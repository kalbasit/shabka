;;; extensions.el --- my-mu4e Layer extensions File for Spacemacs
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

(setq my-mu4e-pre-extensions
      '(
        ;; pre extension names go here
        mu4e-path
        ))

(setq my-mu4e-post-extensions
      '(
        ;; post extension names go here
        ))

;; For each extension, define a function my-mu4e/init-<extension-name>
;;
(defun my-mu4e/init-mu4e-path ()
  "Initialize my extension"
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
