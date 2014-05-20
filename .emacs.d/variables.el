;; Decalre some global variables
(defvar emacs-cache-dir
  (f-expand "cache" user-emacs-directory))
(unless (f-directory? emacs-cache-dir)
  (f-mkdir emacs-cache-dir))
