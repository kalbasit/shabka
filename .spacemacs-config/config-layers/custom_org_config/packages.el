;;; packages.el --- custom_org_config Layer packages File for Spacemacs
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

(defvar custom_org_config-packages
  '(
    org
    org-ac
    ;; package custom_org_configs go here
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar custom_org_config-excluded-packages '()
  "List of packages to exclude.")

;; For each package, define a function custom_org_config/init-<package-custom_org_config>
;;

(defun custom_org_config/init-org-ac()
  (use-package org-ac
    :defer t
    :config
    (progn
      (org-ac/config-default)
      )
    )

  )



(defun custom_org_config/init-org ()
;;   "Initialize my package"
;;load exporters for odt and texinfo - new in org 8
  (use-package org
    :defer t
    :commands (org-mode
               org-edit-src-exit
               org-agenda
               org-capture
               bh/punch-in
               bh/punch-out
               org-store-link
               org-agenda
               org-iswitchb
               org-clock-goto
               org-clock-in
               bh/org-todo
               bh/widen
               bh/clock-in-last-task)
    :init
    (progn
      (evil-leader/set-key "m'" 'org-edit-src-exit)

      ;; set org agenda global
      (evil-leader/set-key "oo" 'org-agenda)
      (evil-leader/set-key "oc" 'org-capture)

      ;; set punch in and out keys

      (evil-leader/set-key "oI" 'bh/punch-in)
      (evil-leader/set-key "oO" 'bh/punch-out)
      ;; Custom Key Bindings
      (global-set-key "\C-cl" 'org-store-link)
      (global-set-key "\C-ca" 'org-agenda)
      (global-set-key "\C-cb" 'org-iswitchb)
      (global-set-key (kbd "C-c c") 'org-capture)
      (global-set-key (kbd "<f12>") 'org-agenda)
      (global-set-key (kbd "<f11>") 'org-clock-goto)
      (global-set-key (kbd "C-<f11>") 'org-clock-in)
      (global-set-key (kbd "<f5>") 'bh/org-todo)
      (global-set-key (kbd "<S-f5>") 'bh/widen)
      (global-set-key (kbd "<f9> I") 'bh/punch-in)
      (global-set-key (kbd "<f9> O") 'bh/punch-out)
      (global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
      )
    :config
    (progn
      (require 'org-pelican)
      (setq org-src-fontify-natively 1)
      (setq org-agenda-span 'day)
      (setq org-default-notes-file "~/Dropbox/org/refile.org")


      (setq org-directory "~/Dropbox/org")
      (setq org-agenda-files (quote ("~/Dropbox/org"
                                     "~/Dropbox/Arbeit/organisation"
                                     "~/Dropbox/Arbeit/organisation/projects")))

      (setq org-agenda-persistent-filter t)
      (require 'ox-odt)
      (require 'ox-texinfo)
      (require 'ox-beamer)
      (require 'ox-html)
      (require 'ox-md)
      (require 'ox-reveal)

      ;; any headline with level <= 2 is a target
      (setq org-refile-targets '((nil :maxlevel . 2)
                                      ; all top-level headlines in the
                                      ; current buffer are used (first) as a
                                      ; refile target
                                 (org-agenda-files :maxlevel . 2)))

      ;; provide refile targets as paths, including the file name
      ;; (without directory) as level 1 of the path
      (setq org-refile-use-outline-path 'file)

      ;; allow to create new nodes (must be confirmed by the user) as
      ;; refile targets
      (setq org-refile-allow-creating-parent-nodes 'confirm)

      ;; refile only within the current buffer
      (defun my/org-refile-within-current-buffer ()
        "Move the entry at point to another heading in the current buffer."
        (interactive)
        (let ((org-refile-targets '((nil :maxlevel . 5))))
          (org-refile)))
      ;; enable helm org refile into subsection of agenda file
      (setq org-outline-path-complete-in-steps nil)

      ;;(add-hook 'org-mode-hook 'turn-on-org-cdlatex)
      (add-hook 'org-mode-hook 'org-indent-mode)
      (add-hook 'org-mode-hook 'auto-fill-mode)

      (require 'ox-latex)
      (setq org-latex-listings 'minted)
      ;; setup minted to have frame, small text and line numbers
      (setq org-latex-minted-options
                 '(("frame" "lines")
                   ("fontsize" "\\scriptsize")
                   ("linenos" "")))

      ;; setup of latex processing
      (setq org-latex-pdf-process
         (quote
          ("pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "bibtex %b"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f"
           "pdflatex -interaction nonstopmode -shell-escape -output-directory %o %f")))
      (setq org-latex-table-caption-above nil)
      (setq org-html-table-caption-above nil)
      (add-to-list 'org-latex-classes
                   '("article"
                     "\\documentclass{article}
      \\usepackage{geometry}
      \\geometry{a4paper, textwidth=6.5in, textheight=10in,
                  marginparsep=7pt, marginparwidth=.6in}
      \\usepackage{tabulary}
      \\usepackage{minted}
      \\usepackage{natbib}"
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((python . t)
         (ditaa . t)
         (R . t)
         (shell .t))
       )

      (setq org-confirm-babel-evaluate nil)
      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
                    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))


      ;;(setq org-todo-keyword-faces
      ;;      (quote (("TODO" :foreground "red" :weight bold)
      ;;              ("NEXT" :foreground "blue" :weight bold)
      ;;              ("DONE" :foreground "forest green" :weight bold)
      ;;              ("WAITING" :foreground "orange" :weight bold)
      ;;              ("HOLD" :foreground "magenta" :weight bold)
      ;;              ("CANCELLED" :foreground "forest green" :weight bold)
      ;;              ("MEETING" :foreground "forest green" :weight bold)
      ;;              ("PHONE" :foreground "forest green" :weight bold))))

      (setq org-use-fast-todo-selection t)
      ;; if state is changed using shift then no dates or notes are recorded
      (setq org-treat-S-cursor-todo-selection-as-state-change nil)
      ;; set tags according to state of the task

      (setq org-agenda-diary-file "~/Dropbox/org/diary.org")


      ;; Diary
      (require 'holidays)
      (setq holiday-austria-holidays '((holiday-fixed  1  1 "Neujahr")
                                       (holiday-fixed  1  6 "Heilige Drei Könige")
                                       (holiday-easter-etc 1 "Ostermontag")
                                       (holiday-fixed  5  1 "Staatsfeiertag")
                                       (holiday-easter-etc 39 "Christi Himmelfahrt")
                                       (holiday-easter-etc 50 "Pfingstmontag")
                                       (holiday-easter-etc 60 "Fronleichnam")
                                       (holiday-fixed  8 15 "Mariä Himmelfahrt")
                                       (holiday-fixed 10 26 "Nationalfeiertag")
                                       (holiday-fixed 11  1 "Allerheiligen")
                                       (holiday-fixed 12  8 "Maria Empfängnis")
                                       (holiday-fixed 12 25 "Erster Weihnachtstag")
                                       (holiday-fixed 12 26 "Zweiter Weihnachtstag")))
      (setq holiday-local-holidays holiday-austria-holidays)
      (setq calendar-holidays (append holiday-local-holidays holiday-other-holidays))

      (setq org-todo-state-tags-triggers
            (quote (("CANCELLED" ("CANCELLED" . t))
                    ("WAITING" ("WAITING" . t))
                    ("HOLD" ("WAITING") ("HOLD" . t))
                    (done ("WAITING") ("HOLD"))
                    ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))
      ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
      (setq org-capture-templates
            (quote (("t" "todo" entry (file "~/Dropbox/org/refile.org")
                     "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                    ("r" "respond" entry (file "~/Dropbox/org/refile.org")
                     "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                    ("n" "note" entry (file "~/Dropbox/org/refile.org")
                     "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                    ("j" "Journal" entry (file+datetree "~/Dropbox/org/diary.org")
                     "* %?\n%U\n" :clock-in t :clock-resume t)
                    ("w" "org-protocol" entry (file "~/Dropbox/org/refile.org")
                     "* TODO Review %c\n%U\n" :immediate-finish t)
                    ("m" "Meeting" entry (file "~/Dropbox/org/refile.org")
                     "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                    ("p" "Phone call" entry (file "~/Dropbox/org/refile.org")
                     "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                    ("h" "Habit" entry (file "~/Dropbox/org/refile.org")
                     "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
      ;; Remove empty LOGBOOK drawers on clock out
      (defun bh/remove-empty-drawer-on-clock-out ()
        (interactive)
        (save-excursion
          (beginning-of-line 0)
          (org-remove-empty-drawer-at (point))))

      (add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

      ;;;; Refile settings
      ; Exclude DONE state tasks from refile targets
      (defun bh/verify-refile-target ()
        "Exclude todo keywords with a done state from refile targets"
        (not (member (nth 2 (org-heading-components)) org-done-keywords)))

      (setq org-refile-target-verify-function 'bh/verify-refile-target)

      ;; Do not dim blocked tasks
      (setq org-agenda-dim-blocked-tasks nil)

      ;; Compact the block agenda view
      (setq org-agenda-compact-blocks t)

      ; Enable habit tracking (and a bunch of other modules)
      (setq org-modules (quote (org-bbdb
                                org-bibtex
                                org-crypt
                                org-gnus
                                org-id
                                org-info
                                org-jsinfo
                                org-habit
                                org-inlinetask
                                org-irc
                                org-mew
                                org-mhe
                                org-protocol
                                org-rmail
                                org-vm
                                org-wl
                                org-w3m)))

      (require 'org-habit)
      (defun bh/org-todo (arg)
        (interactive "p")
        (if (equal arg 4)
            (save-restriction
              (bh/narrow-to-org-subtree)
              (org-show-todo-tree nil))
          (bh/narrow-to-org-subtree)
          (org-show-todo-tree nil)))


      (defun bh/widen ()
        (interactive)
        (if (equal major-mode 'org-agenda-mode)
            (progn
              (org-agenda-remove-restriction-lock)
              (when org-agenda-sticky
                (org-agenda-redo)))
          (widen)))

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "W" (lambda () (interactive) (setq bh/hide-scheduled-and-waiting-next-tasks t) (bh/widen))))
                'append)

      (defun bh/restrict-to-file-or-follow (arg)
        "Set agenda restriction to 'file or with argument invoke follow mode.
      I don't use follow mode very often but I restrict to file all the time
      so change the default 'F' binding in the agenda to allow both"
        (interactive "p")
        (if (equal arg 4)
            (org-agenda-follow-mode)
          (widen)
          (bh/set-agenda-restriction-lock 4)
          (org-agenda-redo)
          (beginning-of-buffer)))

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
                'append)

      (defun bh/narrow-to-org-subtree ()
        (widen)
        (org-narrow-to-subtree)
        (save-restriction
          (org-agenda-set-restriction-lock)))

      (defun bh/narrow-to-subtree ()
        (interactive)
        (if (equal major-mode 'org-agenda-mode)
            (progn
              (org-with-point-at (org-get-at-bol 'org-hd-marker)
                (bh/narrow-to-org-subtree))
              (when org-agenda-sticky
                (org-agenda-redo)))
          (bh/narrow-to-org-subtree)))

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
                'append)

      (defun bh/narrow-up-one-org-level ()
        (widen)
        (save-excursion
          (outline-up-heading 1 'invisible-ok)
          (bh/narrow-to-org-subtree)))

      (defun bh/get-pom-from-agenda-restriction-or-point ()
        (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
            (org-get-at-bol 'org-hd-marker)
            (and (equal major-mode 'org-mode) (point))
            org-clock-marker))

      (defun bh/narrow-up-one-level ()
        (interactive)
        (if (equal major-mode 'org-agenda-mode)
            (progn
              (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
                (bh/narrow-up-one-org-level))
              (org-agenda-redo))
          (bh/narrow-up-one-org-level)))

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
                'append)

      (defun bh/narrow-to-org-project ()
        (widen)
        (save-excursion
          (bh/find-project-task)
          (bh/narrow-to-org-subtree)))

      (defun bh/narrow-to-project ()
        (interactive)
        (if (equal major-mode 'org-agenda-mode)
            (progn
              (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
                (bh/narrow-to-org-project)
                (save-excursion
                  (bh/find-project-task)
                  (org-agenda-set-restriction-lock)))
              (org-agenda-redo)
              (beginning-of-buffer))
          (bh/narrow-to-org-project)
          (save-restriction
            (org-agenda-set-restriction-lock))))

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
                'append)

      (defvar bh/project-list nil)

      (defun bh/view-next-project ()
        (interactive)
        (let (num-project-left current-project)
          (unless (marker-position org-agenda-restrict-begin)
            (goto-char (point-min))
            ; Clear all of the existing markers on the list
            (while bh/project-list
              (set-marker (pop bh/project-list) nil))
            (re-search-forward "Tasks to Refile")
            (forward-visible-line 1))

          ; Build a new project marker list
          (unless bh/project-list
            (while (< (point) (point-max))
              (while (and (< (point) (point-max))
                          (or (not (org-get-at-bol 'org-hd-marker))
                              (org-with-point-at (org-get-at-bol 'org-hd-marker)
                                (or (not (bh/is-project-p))
                                    (bh/is-project-subtree-p)))))
                (forward-visible-line 1))
              (when (< (point) (point-max))
                (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
              (forward-visible-line 1)))

          ; Pop off the first marker on the list and display
          (setq current-project (pop bh/project-list))
          (when current-project
            (org-with-point-at current-project
              (setq bh/hide-scheduled-and-waiting-next-tasks nil)
              (bh/narrow-to-project))
            ; Remove the marker
            (setq current-project nil)
            (org-agenda-redo)
            (beginning-of-buffer)
            (setq num-projects-left (length bh/project-list))
            (if (> num-projects-left 0)
                (message "%s projects left to view" num-projects-left)
              (beginning-of-buffer)
              (setq bh/hide-scheduled-and-waiting-next-tasks t)
              (error "All projects viewed.")))))

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
                'append)

      (setq org-show-entry-below (quote ((default))))

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'bh/set-agenda-restriction-lock))
                'append)

      (defun bh/set-agenda-restriction-lock (arg)
        "Set restriction lock to current task subtree or file if prefix is specified"
        (interactive "p")
        (let* ((pom (bh/get-pom-from-agenda-restriction-or-point))
               (tags (org-with-point-at pom (org-get-tags-at))))
          (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
            (save-restriction
              (cond
               ((and (equal major-mode 'org-agenda-mode) pom)
                (org-with-point-at pom
                  (org-agenda-set-restriction-lock restriction-type))
                (org-agenda-redo))
               ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
                (org-agenda-set-restriction-lock 'file))
               (pom
                (org-with-point-at pom
                  (org-agenda-set-restriction-lock restriction-type))))))))

      ;; Limit restriction lock highlighting to the headline only
      (setq org-agenda-restriction-lock-highlight-subtree nil)

      ;; Always hilight the current agenda line
      (add-hook 'org-agenda-mode-hook
                '(lambda () (hl-line-mode 1))
                'append)

      ;; Keep tasks with dates on the global todo lists
      (setq org-agenda-todo-ignore-with-date nil)

      ;; Keep tasks with deadlines on the global todo lists
      (setq org-agenda-todo-ignore-deadlines nil)

      ;; Keep tasks with scheduled dates on the global todo lists
      (setq org-agenda-todo-ignore-scheduled nil)

      ;; Keep tasks with timestamps on the global todo lists
      (setq org-agenda-todo-ignore-timestamp nil)

      ;; Remove completed deadline tasks from the agenda view
      (setq org-agenda-skip-deadline-if-done t)

      ;; Remove completed scheduled tasks from the agenda view
      (setq org-agenda-skip-scheduled-if-done t)

      ;; Remove completed items from search results
      (setq org-agenda-skip-timestamp-if-done t)

      (defun bh/find-project-task ()
        "Move point to the parent (project) task if any"
        (save-restriction
          (widen)
          (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
            (while (org-up-heading-safe)
              (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                (setq parent-task (point))))
            (goto-char parent-task)
            parent-task)))

      (defun bh/is-project-p ()
        "Any task with a todo keyword subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task has-subtask))))

      (defun bh/is-project-subtree-p ()
        "Any task with a todo keyword that is in a project subtree.
      Callers of this function already widen the buffer view."
        (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                    (point))))
          (save-excursion
            (bh/find-project-task)
            (if (equal (point) task)
                nil
              t))))

      (defun bh/is-task-p ()
        "Any task with a todo keyword and no subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task (not has-subtask)))))

      (defun bh/is-subproject-p ()
        "Any task which is a subtask of another project"
        (let ((is-subproject)
              (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
          (save-excursion
            (while (and (not is-subproject) (org-up-heading-safe))
              (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                (setq is-subproject t))))
          (and is-a-task is-subproject)))

      (defun bh/list-sublevels-for-projects-indented ()
        "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
        This is normally used by skipping functions where this variable is already local to the agenda."
        (if (marker-buffer org-agenda-restrict-begin)
            (setq org-tags-match-list-sublevels 'indented)
          (setq org-tags-match-list-sublevels nil))
        nil)

      (defun bh/list-sublevels-for-projects ()
        "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
        This is normally used by skipping functions where this variable is already local to the agenda."
        (if (marker-buffer org-agenda-restrict-begin)
            (setq org-tags-match-list-sublevels t)
          (setq org-tags-match-list-sublevels nil))
        nil)

      (defvar bh/hide-scheduled-and-waiting-next-tasks t)

      (defun bh/toggle-next-task-display ()
        (interactive)
        (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
        (when  (equal major-mode 'org-agenda-mode)
          (org-agenda-redo))
        (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

      (defun bh/skip-stuck-projects ()
        "Skip trees that are not stuck projects"
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (if (bh/is-project-p)
                (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                       (has-next ))
                  (save-excursion
                    (forward-line 1)
                    (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                      (unless (member "WAITING" (org-get-tags-at))
                        (setq has-next t))))
                  (if has-next
                      nil
                    next-headline)) ; a stuck project, has subtasks but no next task
              nil))))

      (defun bh/skip-non-stuck-projects ()
        "Skip trees that are not stuck projects"
        ;; (bh/list-sublevels-for-projects-indented)
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (if (bh/is-project-p)
                (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                       (has-next ))
                  (save-excursion
                    (forward-line 1)
                    (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                      (unless (member "WAITING" (org-get-tags-at))
                        (setq has-next t))))
                  (if has-next
                      next-headline
                    nil)) ; a stuck project, has subtasks but no next task
              next-headline))))

      (defun bh/skip-non-projects ()
        "Skip trees that are not projects"
        ;; (bh/list-sublevels-for-projects-indented)
        (if (save-excursion (bh/skip-non-stuck-projects))
            (save-restriction
              (widen)
              (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                (cond
                 ((bh/is-project-p)
                  nil)
                 ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
                  nil)
                 (t
                  subtree-end))))
          (save-excursion (org-end-of-subtree t))))

      (defun bh/skip-project-trees-and-habits ()
        "Skip trees that are projects"
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((bh/is-project-p)
              subtree-end)
             ((org-is-habit-p)
              subtree-end)
             (t
              nil)))))

      (defun bh/skip-projects-and-habits-and-single-tasks ()
        "Skip trees that are projects, tasks that are habits, single non-project tasks"
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (cond
             ((org-is-habit-p)
              next-headline)
             ((and bh/hide-scheduled-and-waiting-next-tasks
                   (member "WAITING" (org-get-tags-at)))
              next-headline)
             ((bh/is-project-p)
              next-headline)
             ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
              next-headline)
             (t
              nil)))))

      (defun bh/skip-project-tasks-maybe ()
        "Show tasks related to the current restriction.
      When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
      When not restricted, skip project and sub-project tasks, habits, and project related tasks."
        (save-restriction
          (widen)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (next-headline (save-excursion (or (outline-next-heading) (point-max))))
                 (limit-to-project (marker-buffer org-agenda-restrict-begin)))
            (cond
             ((bh/is-project-p)
              next-headline)
             ((org-is-habit-p)
              subtree-end)
             ((and (not limit-to-project)
                   (bh/is-project-subtree-p))
              subtree-end)
             ((and limit-to-project
                   (bh/is-project-subtree-p)
                   (member (org-get-todo-state) (list "NEXT")))
              subtree-end)
             (t
              nil)))))

      (defun bh/skip-project-tasks ()
        "Show non-project tasks.
      Skip project and sub-project tasks, habits, and project related tasks."
        (save-restriction
          (widen)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((bh/is-project-p)
              subtree-end)
             ((org-is-habit-p)
              subtree-end)
             ((bh/is-project-subtree-p)
              subtree-end)
             (t
              nil)))))

      (defun bh/skip-non-project-tasks ()
        "Show project tasks.
      Skip project and sub-project tasks, habits, and loose non-project tasks."
        (save-restriction
          (widen)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (cond
             ((bh/is-project-p)
              next-headline)
             ((org-is-habit-p)
              subtree-end)
             ((and (bh/is-project-subtree-p)
                   (member (org-get-todo-state) (list "NEXT")))
              subtree-end)
             ((not (bh/is-project-subtree-p))
              subtree-end)
             (t
              nil)))))

      (defun bh/skip-projects-and-habits ()
        "Skip trees that are projects and tasks that are habits"
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((bh/is-project-p)
              subtree-end)
             ((org-is-habit-p)
              subtree-end)
             (t
              nil)))))

      (defun bh/skip-non-subprojects ()
        "Skip trees that are not projects"
        (let ((next-headline (save-excursion (outline-next-heading))))
          (if (bh/is-subproject-p)
              nil
            next-headline)))

      (setq org-archive-mark-done nil)
      (setq org-archive-location "%s_archive::* Archived Tasks")

      (defun bh/skip-non-archivable-tasks ()
        "Skip trees that are not available for archiving"
        (save-restriction
          (widen)
          ;; Consider only tasks with done todo headings as archivable candidates
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
                (subtree-end (save-excursion (org-end-of-subtree t))))
            (if (member (org-get-todo-state) org-todo-keywords-1)
                (if (member (org-get-todo-state) org-done-keywords)
                    (let* ((daynr (string-to-int (format-time-string "%d" (current-time))))
                           (a-month-ago (* 60 60 24 (+ daynr 1)))
                           (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                           (this-month (format-time-string "%Y-%m-" (current-time)))
                           (subtree-is-current (save-excursion
                                                 (forward-line 1)
                                                 (and (< (point) subtree-end)
                                                      (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                      (if subtree-is-current
                          subtree-end ; Has a date in this month or last month, skip it
                        nil))  ; available to archive
                  (or subtree-end (point-max)))
              next-headline))))
      ;; Custom agenda command definitions
      (setq org-agenda-custom-commands
            (quote (("N" "Notes" tags "NOTE"
                     ((org-agenda-overriding-header "Notes")
                      (org-tags-match-list-sublevels t)))
                    ("h" "Habits" tags-todo "STYLE=\"habit\""
                     ((org-agenda-overriding-header "Habits")
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
                    ("A" "Agenda"
                     ((agenda "" nil)
                      (tags "REFILE"
                            ((org-agenda-overriding-header "Tasks to Refile")
                             (org-tags-match-list-sublevels nil)))
                      (tags-todo "-CANCELLED/!"
                                 ((org-agenda-overriding-header "Stuck Projects")
                                  (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                                  (org-agenda-sorting-strategy
                                   '(category-keep))))
                      (tags-todo "-HOLD-CANCELLED/!"
                                 ((org-agenda-overriding-header "Projects")
                                  (org-agenda-skip-function 'bh/skip-non-projects)
                                  (org-tags-match-list-sublevels 'indented)
                                  (org-agenda-sorting-strategy
                                   '(category-keep))))
                      (tags-todo "-CANCELLED/!NEXT"
                                 ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                        (if bh/hide-scheduled-and-waiting-next-tasks
                                                                            ""
                                                                          " (including WAITING and SCHEDULED tasks)")))
                                  (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                  (org-tags-match-list-sublevels t)
                                  (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-sorting-strategy
                                   '(todo-state-down effort-up category-keep))))
                      (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                 ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                        (if bh/hide-scheduled-and-waiting-next-tasks
                                                                            ""
                                                                          " (including WAITING and SCHEDULED tasks)")))
                                  (org-agenda-skip-function 'bh/skip-non-project-tasks)
                                  (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-sorting-strategy
                                   '(category-keep))))
                      (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                 ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                        (if bh/hide-scheduled-and-waiting-next-tasks
                                                                            ""
                                                                          " (including WAITING and SCHEDULED tasks)")))
                                  (org-agenda-skip-function 'bh/skip-project-tasks)
                                  (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-sorting-strategy
                                   '(category-keep))))
                      (tags-todo "-CANCELLED+WAITING|HOLD/!"
                                 ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                        (if bh/hide-scheduled-and-waiting-next-tasks
                                                                            ""
                                                                          " (including WAITING and SCHEDULED tasks)")))
                                  (org-agenda-skip-function 'bh/skip-non-tasks)
                                  (org-tags-match-list-sublevels nil)
                                  (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                  (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                      (tags "-REFILE/"
                            ((org-agenda-overriding-header "Tasks to Archive")
                             (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                             (org-tags-match-list-sublevels nil))))
                     nil))))


      ;; Include agenda archive files when searching for things
      (setq org-agenda-text-search-extra-files (quote (agenda-archives)))

      ;; Show all future entries for repeating tasks
      (setq org-agenda-repeating-timestamp-show-all t)

      ;; Show all agenda dates - even if they are empty
      (setq org-agenda-show-all-dates t)

      ;; Sorting order for tasks on the agenda
      (setq org-agenda-sorting-strategy
            (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
                    (todo category-up effort-up)
                    (tags category-up effort-up)
                    (search category-up))))

      ;; Start the weekly agenda on Monday
      (setq org-agenda-start-on-weekday 1)

      ;; Enable display of the time grid so we can see the marker for the current time
      (setq org-agenda-time-grid (quote ((daily today remove-match)
                                         #("----------------" 0 16 (org-heading t))
                                         (0900 1100 1300 1500 1700))))

      ;; Display tags farther right
      (setq org-agenda-tags-column -102)

      ;;
      ;; Agenda sorting functions
      ;;
      (setq org-agenda-cmp-user-defined 'bh/agenda-sort)

      (defun bh/agenda-sort (a b)
        "Sorting strategy for agenda items.
      Late deadlines first, then scheduled, then non-late deadlines"
        (let (result num-a num-b)
          (cond
           ; time specific items are already sorted first by org-agenda-sorting-strategy

           ; non-deadline and non-scheduled items next
           ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

           ; deadlines for today next
           ((bh/agenda-sort-test 'bh/is-due-deadline a b))

           ; late deadlines next
           ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

           ; scheduled items for today next
           ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

           ; late scheduled items next
           ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

           ; pending deadlines last
           ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

           ; finally default to unsorted
           (t (setq result nil)))
          result))

      (defmacro bh/agenda-sort-test (fn a b)
        "Test for agenda sort"
        `(cond
          ; if both match leave them unsorted
          ((and (apply ,fn (list ,a))
                (apply ,fn (list ,b)))
           (setq result nil))
          ; if a matches put a first
          ((apply ,fn (list ,a))
           (setq result -1))
          ; otherwise if b matches put b first
          ((apply ,fn (list ,b))
           (setq result 1))
          ; if none match leave them unsorted
          (t nil)))

      (defmacro bh/agenda-sort-test-num (fn compfn a b)
        `(cond
          ((apply ,fn (list ,a))
           (setq num-a (string-to-number (match-string 1 ,a)))
           (if (apply ,fn (list ,b))
               (progn
                 (setq num-b (string-to-number (match-string 1 ,b)))
                 (setq result (if (apply ,compfn (list num-a num-b))
                                  -1
                                1)))
             (setq result -1)))
          ((apply ,fn (list ,b))
           (setq result 1))
          (t nil)))

      (defun bh/is-not-scheduled-or-deadline (date-str)
        (and (not (bh/is-deadline date-str))
             (not (bh/is-scheduled date-str))))

      (defun bh/is-due-deadline (date-str)
        (string-match "Deadline:" date-str))

      (defun bh/is-late-deadline (date-str)
        (string-match "\\([0-9]*\\) d\. ago:" date-str))

      (defun bh/is-pending-deadline (date-str)
        (string-match "In \\([^-]*\\)d\.:" date-str))

      (defun bh/is-deadline (date-str)
        (or (bh/is-due-deadline date-str)
            (bh/is-late-deadline date-str)
            (bh/is-pending-deadline date-str)))

      (defun bh/is-scheduled (date-str)
        (or (bh/is-scheduled-today date-str)
            (bh/is-scheduled-late date-str)))

      (defun bh/is-scheduled-today (date-str)
        (string-match "Scheduled:" date-str))

      (defun bh/is-scheduled-late (date-str)
        (string-match "Sched\.\\(.*\\)x:" date-str))

      ;; Use sticky agenda's so they persist
      (setq org-agenda-sticky t)

      ;; clocking in

      ;; Resume clocking task when emacs is restarted
      (org-clock-persistence-insinuate)
      ;;
      ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
      (setq org-clock-history-length 23)
      ;; Resume clocking task on clock-in if the clock is open
      (setq org-clock-in-resume t)
      ;; Change tasks to NEXT when clocking in
      (setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
      ;; Separate drawers for clocking and logs
      (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
      ;; Save clock data and state changes and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t)
      ;; Clock out when moving task to a done state
      (setq org-clock-out-when-done t)
      ;; Save the running clock and all clock history when exiting Emacs, load it on startup
      (setq org-clock-persist t)
      ;; Do not prompt to resume an active clock
      (setq org-clock-persist-query-resume nil)
      ;; Enable auto clock resolution for finding open clocks
      (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
      ;; Include current clocking task in clock reports
      (setq org-clock-report-include-clocking-task t)

      (setq bh/keep-clock-running nil)

      (defun bh/clock-in-to-next (kw)
        "Switch a task from TODO to NEXT when clocking in.
      Skips capture tasks, projects, and subprojects.
      Switch projects and subprojects from NEXT back to TODO"
        (when (not (and (boundp 'org-capture-mode) org-capture-mode))
          (cond
           ((and (member (org-get-todo-state) (list "TODO"))
                 (bh/is-task-p))
            "NEXT")
           ((and (member (org-get-todo-state) (list "NEXT"))
                 (bh/is-project-p))
            "TODO"))))


      (defun bh/punch-in (arg)
        "Start continuous clocking and set the default task to the
      selected task.  If no task is selected set the Organization task
      as the default task."
        (interactive "p")
        (setq bh/keep-clock-running t)
        (if (equal major-mode 'org-agenda-mode)
            ;;
            ;; We're in the agenda
            ;;
            (let* ((marker (org-get-at-bol 'org-hd-marker))
                   (tags (org-with-point-at marker (org-get-tags-at))))
              (if (and (eq arg 4) tags)
                  (org-agenda-clock-in '(16))
                (bh/clock-in-organization-task-as-default)))
          ;;
          ;; We are not in the agenda
          ;;
          (save-restriction
            (widen)
            ; Find the tags on the current task
            (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
                (org-clock-in '(16))
              (bh/clock-in-organization-task-as-default)))))

      (defun bh/punch-out ()
        (interactive)
        (setq bh/keep-clock-running nil)
        (when (org-clock-is-active)
          (org-clock-out))
        (org-agenda-remove-restriction-lock))

      (defun bh/clock-in-default-task ()
        (save-excursion
          (org-with-point-at org-clock-default-task
            (org-clock-in))))

      (defun bh/clock-in-parent-task ()
        "Move point to the parent (project) task if any and clock in"
        (let ((parent-task))
          (save-excursion
            (save-restriction
              (widen)
              (while (and (not parent-task) (org-up-heading-safe))
                (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                  (setq parent-task (point))))
              (if parent-task
                  (org-with-point-at parent-task
                    (org-clock-in))
                (when bh/keep-clock-running
                  (bh/clock-in-default-task)))))))

      (defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

      (defun bh/clock-in-organization-task-as-default ()
        (interactive)
        (org-with-point-at (org-id-find bh/organization-task-id 'marker)
          (org-clock-in '(16))))

      (defun bh/clock-out-maybe ()
        (when (and bh/keep-clock-running
                   (not org-clock-clocking-in)
                   (marker-buffer org-clock-default-task)
                   (not org-clock-resolving-clocks-due-to-idleness))
          (bh/clock-in-parent-task)))

      (add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

      (require 'org-id)
      (defun bh/clock-in-task-by-id (id)
        "Clock in a task by id"
        (org-with-point-at (org-id-find id 'marker)
          (org-clock-in nil)))

      (defun bh/clock-in-last-task (arg)
        "Clock in the interrupted task if there is one
      Skip the default task and get the next one.
      A prefix arg forces clock in of the default task."
        (interactive "p")
        (let ((clock-in-to-task
               (cond
                ((eq arg 4) org-clock-default-task)
                ((and (org-clock-is-active)
                      (equal org-clock-default-task (cadr org-clock-history)))
                 (caddr org-clock-history))
                ((org-clock-is-active) (cadr org-clock-history))
                ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
                (t (car org-clock-history)))))
          (widen)
          (org-with-point-at clock-in-to-task
            (org-clock-in nil))))

      (setq org-time-stamp-rounding-minutes (quote (1 1)))

      (setq org-agenda-clock-consistency-checks
            (quote (:max-duration "4:00"
                    :min-duration 0
                    :max-gap 0
                    :gap-ok-around ("4:00"))))

      ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t)

      ;; Agenda clock report parameters
      (setq org-agenda-clockreport-parameter-plist
            (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
    )
  )



)
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
