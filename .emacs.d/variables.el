;; Decalre some global variables
(defvar emacs-cache-dir
  (f-expand "cache" user-emacs-directory))
(unless (f-directory? emacs-cache-dir)
  (f-mkdir emacs-cache-dir))
(defvar emacs-backup-dir
  (f-expand "backup" user-emacs-directory))
(unless (f-directory? emacs-backup-dir)
  (f-mkdir emacs-backup-dir))
(defvar emacs-autosave-dir
  (f-expand "autosave" user-emacs-directory))
(unless (f-directory? emacs-autosave-dir)
  (f-mkdir emacs-autosave-dir))
