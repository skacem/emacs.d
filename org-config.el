;;; org-config.el --- Org Mode Configuration -*- lexical-binding: t -*-
;;
;; This file contains all Org-mode related configurations, including:
;; - Core Org setup
;; - Visual enhancements (bullets, modern styling)
;; - Org-roam for networked notes
;; - Table alignment with valign
;;

;;-----------------------------------------------------------------------------
;; Core Org Mode Configuration
;;-----------------------------------------------------------------------------

(use-package org
  :ensure t
  :config
  ;; Set the default directory where your main Org files are stored.
  (setq org-directory "~/Dev/org")
  ;; Create the org directory if it doesn't exist
  (make-directory org-directory t)
  ;; Set the default file for capturing quick notes using `org-capture` (C-c c).
  ;; This is often used as an inbox file.
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  ;; List the Org files that should be included when generating the Org Agenda view (C-c a a).
  ;; Setting it to `(list org-directory)` includes all `.org` files in that directory.
  ;; You might prefer to list specific files like `("~/.org/agenda.org" "~/.org/tasks.org")`.
  (setq org-agenda-files (list org-directory))

  ;; Configure the visual appearance of Org buffers for readability and immersion.
  (setq org-startup-indented t         ; Indent headings based on their level.
        org-pretty-entities t          ; Display entities like `->` as arrows, `lambda` as λ, etc.
        org-hide-emphasis-markers t    ; Hide the markup characters for bold, italics, etc.
                                       ; (e.g., show **bold** as bold text directly).
        org-startup-with-inline-images t) ; Automatically display images linked in Org files when opened.

  ;; Customize the keywords used for TODO states and their quick keys in the agenda.
  ;; `sequence` defines a linear progression of states. `|` separates active states from done states.
  ;; The letters in parentheses are the keys to press after C-c C-t in the agenda.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; Define templates for quickly capturing new entries using `org-capture` (C-c c).
  ;; Each list item defines a template: (key description type target template-string).
  ;; `entry` creates a new heading. `file+headline` or `file+datetree` specify the destination.
  ;; `%?` places the cursor after the template is inserted. `%i` includes the initial selection.
  ;; `%a` includes annotation (like a link to where capture was initiated). `%U` inserts a timestamp.
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
           "* TODO %?
%i
%a")
          ("n" "Note" entry (file+datetree org-default-notes-file)
           "* %?
Entered on %U
%i
%a")))

  ;; Configure how the Org Agenda view (C-c a a) is generated and displayed.
  (setq org-agenda-include-diary t     ; Include entries from the Emacs diary file in the agenda.
        org-agenda-start-on-weekday nil ; Start the agenda display on the current day, not a fixed day like Monday.
        org-agenda-span 'month         ; Show a month-long view by default in the agenda.
        org-agenda-time-grid nil))     ; Do not display a detailed time grid in the daily agenda view.

;; Set up global Org-mode keybindings
(global-set-key (kbd "C-c a c") 'org-capture)  ; Quick capture
(global-set-key (kbd "C-c a a") 'org-agenda)   ; View agenda

;;-----------------------------------------------------------------------------
;; Visual Enhancements for Org Mode
;;-----------------------------------------------------------------------------

;; Org-bullets replaces the standard outline asterisks (`*`, `**`, etc.) with
;; prettier Unicode bullet characters, enhancing the visual appearance of Org files.
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Modern, opinionated styling for Org mode
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-table nil) ; Disable table styling to avoid potential issues
  :config
  (global-org-modern-mode))

;; Visual table editor for Org mode
(use-package valign
  :hook (org-mode . valign-mode))

;;-----------------------------------------------------------------------------
;; Org-roam for Non-hierarchical Note-taking
;;-----------------------------------------------------------------------------

;; Org-roam for non-hierarchical note-taking with backlinks
(use-package org-roam
  :after org
  :custom
  (org-roam-directory (file-truename "~/Dev/org/roam"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  ;; Create org-roam directory if it doesn't exist
  (make-directory org-roam-directory t)
  (org-roam-setup))

;;-----------------------------------------------------------------------------
;; Function to Generate Today's Org File
;;-----------------------------------------------------------------------------

;; Function to generate the file path for today's daily Org file based on the date.
;; If the file doesn't exist, it creates it with a basic structure.
(defun get-today-org-file ()
  "Get or create an org file for today's date in `org-directory`."
  (let* ((today (format-time-string "%Y-%m-%d")) ; Format the current date as-MM-DD.
         ;; Construct the full file path using the formatted date and `org-directory`.
         (file-path (expand-file-name (concat today ".org") org-directory)))
    ;; Check if the file already exists.
    (unless (file-exists-p file-path)
      ;; If it doesn't exist, create it.
      (with-temp-file file-path ; Create the file in a temporary buffer and save it.
        ;; Insert some initial content into the new file.
        (insert (concat "#+TITLE: Daily Tasks - " today "

"
                       "* Tasks

"
                       "* Notes

"
                       "* Log

"))))
    ;; Return the full path to the daily Org file.
    file-path))

;;-----------------------------------------------------------------------------
;; Additional Customization for Org Mode
;;-----------------------------------------------------------------------------

;; Set up olivetti mode for distraction-free writing specifically for Org mode
(use-package olivetti
  :hook (org-mode . (lambda () 
                      (olivetti-mode 1)
                      (olivetti-set-width 100))))

;; The end of org-config.el
(provide 'org-config)
;;; org-config.el ends here
