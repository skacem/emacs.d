;;; init.el --- Enhanced Immersive Emacs Configuration with CalDAV Sync -*- lexical-binding: t -*-
;;
;; This configuration aims for an immersive, minimalist Emacs experience
;; while providing essential features for development, notes (Org-mode),
;; and calendar synchronization via CalDAV.
;;
;; Each section is commented to explain its purpose and the settings used.

;;-----------------------------------------------------------------------------
;; Performance Optimization
;;-----------------------------------------------------------------------------

;; Increase GC threshold during startup for faster initialization
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset GC threshold after startup to balance performance and memory usage
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216  ; 16MB
                  gc-cons-percentage 0.1)))

;; Collect garbage when Emacs is out of focus
(add-function :after after-focus-change-function
              (defun garbage-collect-maybe ()
                (unless (frame-focus-state)
                  (garbage-collect))))

;; Use a hook so the message doesn't get clobbered by other messages
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Suppress specific minor byte-compile warnings.
(setq byte-compile-warnings
      '(not obsolete not free-vars unresolved callargs
            redefine noruntime cl-functions interactive-only
            make-local))

;;-----------------------------------------------------------------------------
;; Package Management Setup (use-package)
;;-----------------------------------------------------------------------------

;; Ensure the package system is initialized. This is the foundation for
;; downloading, installing, and managing third-party Emacs packages.
(require 'package)

;; Define the list of package archives Emacs should fetch packages from.
;; - melpa: The largest archive, containing many community-contributed packages.
;; - gnu: The official GNU Emacs package archive.
;; - nongnu: Another official archive for packages that don't meet all GNU criteria.
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;; Initialize the package system. This reads the package lists from the
;; configured archives.
(package-initialize)

;; Bootstrap `use-package`. `use-package` is a powerful macro that simplifies
;; package configuration significantly. This block checks if `use-package`
;; is installed, and if not, it downloads and installs it.
(unless (package-installed-p 'use-package)
  ;; Refresh the list of packages to ensure use-package is available for download.
  (package-refresh-contents)
  ;; Install the use-package package.
  (package-install 'use-package))

;; Load the use-package library so its macros are available for use.
(require 'use-package)

;; Configure `use-package` to automatically install any package specified
;; with `use-package` if it's not already installed. This makes your
;; configuration portable across different Emacs installations.
(setq use-package-always-ensure t)

;; Load the newest version of a file, not the version in the byte-compiled file
(setq load-prefer-newer t)

;;-----------------------------------------------------------------------------
;; Personal Information
;;-----------------------------------------------------------------------------

;; Set variables for your full name and email address. These are used by
;; various Emacs features and packages, such as Git commit messages (via Magit),
;; Org-mode exports, and email composition.
(setq user-full-name "Skander"
      user-mail-address "skander.kacem@gmail.com")

;;-----------------------------------------------------------------------------
;; Backups and History
;;-----------------------------------------------------------------------------

;; Configure backup files. Emacs can automatically create backup copies of files
;; before you save changes, which can be a lifesaver.
;; `:ensure nil` is used because `files` is a fundamental, built-in part of Emacs.
(use-package files
  :ensure nil
  :config
  ;; Store backup files in a specific directory within your Emacs configuration
  ;; directory, keeping your source directories clean. The `.` matches any file path.
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
  ;; Keep an unlimited number of old versions of backups (-1).
  (setq delete-old-versions -1
        ;; Enable version control for backup files.
        version-control t
        ;; Make backup files even for files that are under version control (like Git).
        vc-make-backup-files t
        ;; Store auto-save files in a specific directory. Auto-save happens periodically
        ;; while you're editing to prevent data loss if Emacs crashes.
        auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))))

;; Configure saving command history, kill ring (copy/paste history), etc.,
;; so they persist between Emacs sessions.
;; `:ensure nil` because `savehist` is built-in.
(use-package savehist
  :ensure nil
  :config
  ;; Specify the file where history data is saved.
  (setq savehist-file "~/.emacs.d/savehist")
  ;; Set the maximum number of history items to remember for each history list.
  (setq history-length 1000
        ;; Remove duplicate entries from history lists.
        history-delete-duplicates t
        ;; Save the history of commands entered in the minibuffer.
        savehist-save-minibuffer-history 1
        ;; Specify additional variables whose history should be saved.
        ;; `kill-ring` is essential for persistent copy/paste history.
        ;; `search-ring` and `regexp-search-ring` save search history.
        savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
  ;; Enable savehist mode to start saving history.
  (savehist-mode 1))

;; Save cursor position in files
(use-package saveplace
  :ensure nil
  :config
  (setq save-place-file "~/.emacs.d/saveplace")
  (save-place-mode 1))

;; Remember recently edited files
(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file "~/.emacs.d/recentf"
        recentf-max-saved-items 100
        recentf-max-menu-items 15)
  (recentf-mode 1))

;;-----------------------------------------------------------------------------
;; UI Enhancements for Immersion and Minimalism
;;-----------------------------------------------------------------------------

;; Configure core Emacs UI elements to create a minimalist, immersive environment.
;; `:ensure nil` as these are built-in Emacs features.
(use-package emacs
  :ensure nil
  :config
  ;; Disable standard UI elements to maximize the space available for buffers.
  (tool-bar-mode -1)           ; Hide the graphical toolbar.
  (menu-bar-mode -1)           ; Hide the menu bar.
  (scroll-bar-mode -1)         ; Hide the scrollbar.

  ;; Enable helpful visual cues within the buffer area.
  (column-number-mode t)       ; Display the current column number in the mode line.
  (global-display-line-numbers-mode) ; Show line numbers in the left fringe of all buffers.
  (global-hl-line-mode 1)      ; Highlight the entire line where the cursor is located.
  (show-paren-mode t)          ; Highlight the matching parenthesis/bracket to the one at point.
  (setq show-paren-delay 0)     ; Highlight matching parenthesis immediately without delay.
  (setq show-paren-style 'parenthesis) ; Only highlight the parenthesis characters themselves.
  (transient-mark-mode t)      ; Highlight the active region (set with C-SPC) visually.
  (global-font-lock-mode t)    ; Enable syntax highlighting (coloring code, text structure) globally.

  ;; Configure interaction feedback and startup behavior.
  (setq visible-bell t)         ; Flash the screen instead of making a sound on errors.
  (setq inhibit-startup-screen t) ; Prevent the initial splash screen from appearing.
  (setq indicate-empty-lines t) ; Show a visual indicator (usually a vertical line) for empty lines.

  ;; Cursor appearance.
  (setq-default cursor-type 'box) ; Use a box or rectangle cursor shape.
  (blink-cursor-mode 0)        ; Disable the blinking cursor.

  ;; Simplify yes/no prompts in the minibuffer.
  (fset 'yes-or-no-p 'y-or-n-p) ; Allow responding with 'y' or 'n' instead of typing 'yes'/'no'.

  ;; Display the current time and date in the mode line.
  (display-time-mode 1)
  (setq display-time-24hr-format t) ; Use 24-hour format (e.g., 13:00 instead of 1:00 PM).
  (setq display-time-day-and-date t)) ; Include the day and date (e.g., Mon May 20).

;; Improve the mode line
(use-package doom-modeline
  :after all-the-icons
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 4)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-lsp t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-project-detection 'project)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  :config
  (doom-modeline-mode 1))

;; Icon support for UI enhancements
(use-package all-the-icons
  :config
  (when (and (display-graphic-p)
             (not (member "all-the-icons" (font-family-list))))
    (all-the-icons-install-fonts t)))

;; Attempt to create Emacs frames without window manager decorations (title bar, borders).
;; This setting is dependent on your operating system and window manager and may not
;; have an effect in all environments.
(add-to-list 'default-frame-alist '(undecorated . t))

;; Set the frame title format to show the buffer name and project
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        (:eval (if (projectile-project-p)
                   (format " - [%s]" (projectile-project-name))
                 ""))))

;; Enter fullscreen mode automatically when Emacs starts in a graphical session.
;; `noninteractive` is a variable that is true when Emacs is run in batch mode
;; (e.g., from a script) without a graphical interface.
(unless noninteractive
  (toggle-frame-fullscreen))

;; Highlight matching parentheses with different colors based on nesting level
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Display a thin vertical line at column 80 to indicate code width limit
(use-package display-fill-column-indicator
  :ensure nil  ; Built-in since Emacs 27
  :hook (prog-mode . display-fill-column-indicator-mode)
  :custom
  (display-fill-column-indicator-column 80))

;; Dimming non-active windows for better focus
(use-package dimmer
  :custom
  (dimmer-fraction 0.3)
  (dimmer-exclusion-regexp-list '(".*Minibuf.*" ".*which-key.*" ".*NeoTree.*" ".*Messages.*"))
  :config
  (dimmer-mode t))

;; Tab bar for managing buffers with tabs
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :custom
  (centaur-tabs-set-icons t)
  (centaur-tabs-gray-out-icons 'buffer)
  (centaur-tabs-set-bar 'over)
  (centaur-tabs-set-modified-marker t)
  :bind
  (("C-<prior>" . centaur-tabs-backward)
   ("C-<next>" . centaur-tabs-forward)))

;; Distraction-free writing modes
;; Note: Org-specific olivetti configuration is now in org-config.el
(use-package olivetti
  :defer t
  :custom
  (olivetti-body-width 100))


;;-----------------------------------------------------------------------------
;; Basic Editing Experience
;;-----------------------------------------------------------------------------

;; Configure fundamental editing behaviors for convenience.
;; `:ensure nil` as these are built-in features.
(use-package simple
  :ensure nil
  :config
  (delete-selection-mode 1)    ; Typing text replaces the active region (standard behavior in most editors).
  (global-auto-revert-mode 1)  ; Automatically reload buffers if the corresponding file on disk changes.
                               ; Useful when files are modified by external processes or collaborators.
  (save-place-mode 1)          ; Remember and restore the cursor position in files when you revisit them.
  (electric-pair-mode 1))      ; Automatically insert closing delimiters (like `)` after `(`) and match quotes.

;; Enable CUA mode, primarily for its rectangular selection capabilities (C-RET).
;; We disable its rebindings of C-x, C-c, C-v to keep standard Emacs keybindings.
;; The Super key bindings below provide OS-level cut/copy/paste.
;; `:ensure nil` as `cua-base` is built-in.
(use-package cua-base
  :ensure nil
  :config
  (cua-mode t)                  ; Enable CUA mode.
  (setq cua-enable-cua-keys nil)) ; Disable CUA rebindings for C-x, C-c, C-v.

;; Another package for highlighting matching parentheses/brackets.
(use-package highlight-parentheses
  :config
  ;; Enable highlighting parentheses globally in all buffers.
  (global-highlight-parentheses-mode))

;; Better parentheses editing with smartparens
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

;; Enhanced undo system with visualization
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))


;; Edit multiple regions simultaneously
(use-package iedit
  :bind ("C-;" . iedit-mode))

;; Move lines or regions up and down
(use-package move-text
  :bind (("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down)))


;; Highlight indentation for better visualization
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 20))

;; Customize `kill-region` (cut) and `kill-ring-save` (copy) to operate on the
;; entire current line if no region is actively selected. This is a common
;; productivity tweak. Using `advice-add` is the modern way to modify functions.
(defun my-kill-whole-line-if-no-region (orig-fn &rest args)
  "Kill/copy whole line if no region is active, otherwise call original function."
  (interactive
   ;; Check if a region is active (`mark-active` is non-nil if a region is set).
   (if mark-active
       ;; If a region is active, use the region boundaries as arguments for the original function.
       (list (region-beginning) (region-end))
     ;; If no region is active, calculate the start and end of the current line.
     ;; `(line-beginning-position)` gets the character position at the start of the current line.
     ;; `(line-beginning-position 2)` gets the character position at the start of the *next* line.
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;; Add the custom function as advice to the original `kill-region` and `kill-ring-save` functions.
;; `:before-while` means the advice runs *before* the original function. If the advice
;; returns a non-nil value (in this case, the list of arguments), the original function
;; is called non-interactively with those arguments, and the advice's return value
;; is used as the result. If the advice returns nil, the original function is called
;; interactively as usual.
(advice-add 'kill-region :before-while #'my-kill-whole-line-if-no-region)
(advice-add 'kill-ring-save :before-while #'my-kill-whole-line-if-no-region)

;;-----------------------------------------------------------------------------
;; Buffer, Window, and File Management
;;-----------------------------------------------------------------------------

;; Replace the default `list-buffers` command (C-x C-b) with `ibuffer`,
;; a more feature-rich and customizable buffer list.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Improved integration with the system clipboard and the X primary selection.
;; `:ensure nil` as `select` is built-in.
(use-package select
  :ensure nil
  :config
  ;; Enable copying to and pasting from the system clipboard.
  (setq select-enable-clipboard t
        ;; Enable interaction with the X primary selection (highlight text to copy,
        ;; middle-click to paste). Relevant on Linux/Unix systems with X.
        select-enable-primary t
        ;; Save text to the kill ring before pasting from the interprogram clipboard.
        ;; This preserves your kill ring history when pasting from external applications.
        save-interprogram-paste-before-kill t))

;; Manage the history of window configurations, allowing you to step back
;; and forth through previous window layouts using `winner-undo` and `winner-redo`.
;; `:ensure nil` as `winner` is built-in.
(use-package winner
  :ensure nil
  :config
  (winner-mode 1)) ; Enable winner mode.

;; Display the code minimap (like Sublime Text)
(use-package minimap
  :commands minimap-mode
  :custom
  (minimap-window-location 'right)
  (minimap-update-delay 0.2)
  (minimap-minimum-width 15)
  :config 
  ;; Use a-key binding to toggle minimap
  (global-set-key (kbd "C-c m") 'minimap-mode))

;; Workspace management with perspective
(use-package perspective
  :bind (("C-x k" . persp-kill-buffer*) ; Override kill-buffer with perspective version
         ("C-x b" . persp-switch-to-buffer*) ; Override switch-buffer with perspective version
         ("C-x C-b" . persp-ibuffer)) ; Override ibuffer with perspective version
  :custom
  (persp-mode-prefix-key (kbd "C-c w"))
  :config
  (persp-mode))

;;-----------------------------------------------------------------------------
;; Advanced Completion Framework (Ivy, Counsel, Swiper)
;;-----------------------------------------------------------------------------

;; Ivy provides a flexible, interactive completion framework for the minibuffer.
(use-package ivy
  :config
  (ivy-mode 1) ; Enable Ivy mode globally.
  ;; Treat open buffers as virtual files in Ivy's file completion, making it
  ;; easier to switch between buffers using file-finding commands.
  (setq ivy-use-virtual-buffers t)
  ;; Customize the format string displayed in the minibuffer during completion,
  ;; showing the current match number and the total number of matches.
  (setq ivy-count-format "(%d/%d) "))

;; Counsel provides a collection of commands that use Ivy for completion,
;; such as enhanced versions of `M-x` (counsel-M-x), buffer switching (counsel-switch-buffer),
;; file finding (counsel-find-file), etc.
(use-package counsel
  :config
  (counsel-mode 1)) ; Enable Counsel commands.

;; Swiper is an Ivy-based interactive search and replace tool. It's often
;; used as a powerful alternative to the built-in `isearch` (C-s, C-r).
(use-package swiper
  ;; Bind swiper to the standard search key C-s. This replaces the default `isearch-forward`.
  :bind (("C-s" . swiper)))

;; Enhanced minibuffer annotations
(use-package marginalia
  :after ivy
  :config
  (marginalia-mode))

;; Flexible completion style
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles orderless partial-completion)))))

;; Shows available keybindings in popup
(use-package which-key
  :config
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-max-description-length 40)
  (which-key-separator " → "))

;;-----------------------------------------------------------------------------
;; Project Management and Version Control
;;-----------------------------------------------------------------------------

;; Projectile provides a set of tools for working with projects (collections
;; of related files, typically detected by the presence of a version control
;; repository like Git). It offers commands for finding files within a project,
;; switching between projects, running project-specific commands, etc.
(use-package projectile
  :init
  (projectile-mode +1) ; Enable projectile mode globally when Emacs starts (+1 turns it on).
  ;; Define a keymap specifically for projectile commands, often bound to
  ;; a convenient prefix like C-c p.
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)) ; C-c p will open the projectile command menu.
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-indexing-method 'alien))

;; Magit is widely considered the best Git interface, not just in Emacs,
;; but among all Git clients. It provides a clear, interactive status buffer
;; and commands for staging, committing, pushing, pulling, branching, etc.
(use-package magit
  ;; Bind the main Magit status command to C-c g. This opens the status buffer
  ;; for the current project or repository.
  :bind (("C-c g" . magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;-----------------------------------------------------------------------------
;; Language Support with Enhanced Completion
;;-----------------------------------------------------------------------------

;; Company provides a modular in-buffer completion framework.
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  :config
  (global-company-mode))

;; Treesitter for better syntax highlighting
(when (>= emacs-major-version 29)
  (use-package treesit
    :ensure nil  ; Built into Emacs 29+
    :config
    (setq treesit-language-source-alist
          '((python "https://github.com/tree-sitter/tree-sitter-python")
            (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
            (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
            (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
            (json "https://github.com/tree-sitter/tree-sitter-json")
            (html "https://github.com/tree-sitter/tree-sitter-html")
            (css "https://github.com/tree-sitter/tree-sitter-css")
            (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
            (go "https://github.com/tree-sitter/tree-sitter-go")
            (bash "https://github.com/tree-sitter/tree-sitter-bash")
            (rust "https://github.com/tree-sitter/tree-sitter-rust"))))
    ;; Map major modes to their tree-sitter equivalents
    (setq major-mode-remap-alist
          '((python-mode . python-ts-mode)
            (js-mode . js-ts-mode)
            (typescript-mode . typescript-ts-mode)
            (json-mode . json-ts-mode)
            (css-mode . css-ts-mode)
            (c-mode . c-ts-mode)
            (c++-mode . c++-ts-mode)
            (rust-mode . rust-ts-mode))))

;; Configure basic settings for Python mode.
;; `:ensure nil` as `python` mode is a built-in major mode in Emacs.
(use-package python
  :ensure nil
  :config
  ;; Set the standard indentation level for Python code to 4 spaces (PEP 8).
  (setq python-indent-offset 4)
  ;; Specify the Python interpreter to use when you run an Emacs Python shell
  ;; (e.g., using C-c C-p in a Python buffer).
  (setq python-shell-interpreter "python3"))

;; Flycheck provides on-the-fly syntax checking and linting for many programming
;; and markup languages. It uses external tools (linters, compilers) to check
;; your code as you type and highlights errors or warnings in the buffer.
;; This is a highly recommended productivity tool for coding.
(use-package flycheck
  :init
  (global-flycheck-mode)) ; Enable flycheck globally in all relevant buffers.

;; Configure LaTeX mode using AUCTeX, a comprehensive environment for writing
;; TeX and LaTeX documents. It provides syntax highlighting, completion,
;; compilation commands, previewing, etc.
(use-package tex
  :ensure auctex ; The `auctex` package provides the enhanced tex mode.
  :config
  ;; Enable automatic saving of LaTeX files during compilation or other processes.
  (setq TeX-auto-save t
        ;; Allow AUCTeX to parse your LaTeX files to understand their structure
        ;; (sections, labels, citations, etc.).
        TeX-parse-self t
        ;; Set PDF mode as the default output format and viewer.
        TeX-PDF-mode t
        ;; Set TeX-master to nil to allow AUCTeX to automatically detect the
        ;; main document file or prompt you if it can't.
        TeX-master nil))

;; Fix for yasnippet configuration (the original cause of the error)
(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode 1))

;; Yasnippet-snippets provides a large collection of pre-defined snippets
;; for many programming languages and text modes, including Org mode.
(use-package yasnippet-snippets
  :after yasnippet) ; Ensure this package is loaded after yasnippet is initialized.

;;-----------------------------------------------------------------------------
;; Load Module Configurations
;;-----------------------------------------------------------------------------

;; Create the directory for module configurations if it doesn't exist
(let ((modules-dir (expand-file-name "modules" user-emacs-directory)))
  (make-directory modules-dir t)
  
  ;; Write the org-config file if it doesn't exist yet
  (let ((org-config-file (expand-file-name "org-config.el" modules-dir)))
    (unless (file-exists-p org-config-file)
      (with-temp-file org-config-file
        (insert-file-contents (expand-file-name "org-config.el" user-emacs-directory)))))

  ;; Add the modules directory to the load path
  (add-to-list 'load-path modules-dir))

;; Load the org-mode configuration
(require 'org-config)

;;-----------------------------------------------------------------------------
;; Terminal Emulator
;;-----------------------------------------------------------------------------

;; Configure vterm, a fast and full-featured terminal emulator that runs within
;; an Emacs buffer. It supports most terminal applications and features.
;; `:defer t` means the package is loaded only when you explicitly call a vterm
;; command (like `vterm`) or when your custom layout function calls it.
(use-package vterm
  :defer t)

;;-----------------------------------------------------------------------------
;; Theme and Appearance
;;-----------------------------------------------------------------------------

;; Load the Zenburn color theme, which provides a dark, low-contrast color scheme.
(use-package zenburn-theme
  :config
  ;; Load the 'zenburn' theme. The `t` argument suppresses the confirmation
  ;; prompt that Emacs usually shows when loading themes.
  (load-theme 'zenburn t))

;; Add an alternative modern theme - Doom themes
(use-package doom-themes
  :defer t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom treemacs theme (all-the-icons must be installed!)
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

;; Add a theme switcher function for easy theme changing
(defun switch-theme ()
  "Switch between themes interactively."
  (interactive)
  (let* ((themes (mapcar #'symbol-name
                          (custom-available-themes)))
         (theme (completing-read "Load theme: " themes nil t)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern theme) t)))

(global-set-key (kbd "C-c t") 'switch-theme)

;;-----------------------------------------------------------------------------
;; Dashboard - Welcome Screen
;;-----------------------------------------------------------------------------

(use-package dashboard
  :after all-the-icons
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((recents  . 8)
                     (bookmarks . 5)
                     (projects . 5)
                     (agenda . 5)))
  (dashboard-banner-logo-title "Welcome to Enhanced Immersive Emacs")
  (dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook))

;;-----------------------------------------------------------------------------
;; Custom Functions
;;-----------------------------------------------------------------------------

;; Function to generate the file path for today's daily Org file based on the date.
;; This is now defined in org-config.el which should be loaded before this point.

;; Function to reload the entire Emacs configuration file (`init.el`).
;; This is useful for applying changes you've made to your configuration
;; without needing to completely restart Emacs.
(defun reload-config ()
  "Reload the user's Emacs configuration file."
  (interactive) ; Make this function callable interactively (e.g., with M-x reload-config).
  (load-file user-init-file) ; Load the file specified by the `user-init-file` variable (your init.el).
  (message "Emacs configuration reloaded!")) ; Display a confirmation message in the minibuffer.

;; Function to prepare commonly used buffers in the background
;; without disturbing the dashboard layout
(defun prepare-background-buffers ()
  "Create daily org file, terminal and dired buffers in the background."
  (interactive)
  ;; Prepare today's org file in the background
  (with-current-buffer (find-file-noselect (get-today-org-file))
    (bury-buffer))
  ;; Prepare dired in the background
  (with-current-buffer (dired-noselect "~")
    (bury-buffer))
  ;; Prepare terminal in the background
  (when (fboundp 'vterm)
    (with-current-buffer (get-buffer-create "*vterm*")
      (unless (eq major-mode 'vterm-mode)
        (vterm-mode))
      (bury-buffer))))

;; Function to set up a 4-window layout when explicitly requested
(defun setup-quad-layout ()
  "Set up a 4-window layout with dashboard, org, terminal and dired."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (other-window 2)
  (split-window-below)
  (other-window -2)
  (switch-to-buffer "*dashboard*")
  (other-window 1)
  (dired "~")
  (other-window 1)
  (find-file (get-today-org-file))
  (other-window 1)
  (if (fboundp 'vterm) (vterm) (eshell))
  (other-window -3))

;; Just prepare buffers in the background, keep dashboard visible
(add-hook 'dashboard-after-initialize-hook 'prepare-background-buffers)

;; Add keybinding to set up the 4-window layout when desired
(global-set-key (kbd "C-c 4") 'setup-quad-layout)

;;-----------------------------------------------------------------------------
;; Global Key Bindings
;;-----------------------------------------------------------------------------

;; Define global keybindings that are available in all Emacs buffers.

;; Custom commands keybindings.
(global-set-key (kbd "C-c r") 'reload-config)       ; Bind C-c r to reload your Emacs configuration.
(global-set-key (kbd "C-c 4") 'setup-quad-layout)   ; Bind C-c 4 to manually set up the 4-window layout.

;; Org-mode related keybindings.
;; These are now defined in org-config.el

;; Standard OS-like keybindings using the Super (Windows/Cmd) key.
;; These provide familiar cut, copy, paste, and undo operations using the Super key.
(global-set-key (kbd "s-x") 'kill-region)           ; Super+x for Cut (uses kill-region on active region).
(global-set-key (kbd "s-c") 'kill-ring-save)        ; Super+c for Copy (uses kill-ring-save on active region).
(global-set-key (kbd "s-v") 'yank)                  ; Super+v for Paste (uses yank).
(global-set-key (kbd "s-z") 'undo)                  ; Super+z for Undo.
(global-set-key (kbd "s-Z") 'undo-tree-redo)        ; Super+Shift+z for Redo.

;; Quick access keybindings
(global-set-key (kbd "C-c C-r") 'recentf-open-files) ; Open recent files list
(global-set-key (kbd "C-c b") 'bookmark-jump)       ; Jump to bookmark
(global-set-key (kbd "C-c s") 'ispell-word)         ; Spell check word
(global-set-key (kbd "C-c S") 'ispell-buffer)       ; Spell check buffer
(global-set-key (kbd "C-c C-;") 'comment-or-uncomment-region) ; Comment/uncomment region

;; Improved buffer navigation
(global-set-key (kbd "M-o") 'other-window)          ; Faster window switching
(global-set-key (kbd "C-x C-b") 'ibuffer)          ; Better buffer list
(global-set-key (kbd "C-x p") 'previous-buffer)     ; Quick previous buffer
(global-set-key (kbd "C-x n") 'next-buffer)         ; Quick next buffer

;; Search, find, and replace keybindings
(global-set-key (kbd "C-c g") 'grep)               ; Grep (search for text in files)
(global-set-key (kbd "C-c j") 'dumb-jump-go)       ; Jump to definition
(global-set-key (kbd "C-c h") 'highlight-symbol-at-point) ; Highlight current symbol

;; Window navigation with Shift+arrows
(global-set-key (kbd "S-<left>")  'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)
(global-set-key (kbd "S-<up>")    'windmove-up)
(global-set-key (kbd "S-<down>")  'windmove-down)

;;-----------------------------------------------------------------------------
;; Customization File
;;-----------------------------------------------------------------------------

;; Emacs's built-in `customize` system (accessed via M-x customize) saves
;; configuration changes to a separate file, typically `~/.emacs.d/custom.el`.
;; Loading this file here ensures that any settings saved via `customize`
;; are applied and can override defaults set earlier in this `init.el`.
(setq custom-file "~/.emacs.d/custom.el")
;; Check if the custom file exists before trying to load it.
(when (file-exists-p custom-file)
  (load custom-file))

;;-----------------------------------------------------------------------------
;; End of init.el
;;-----------------------------------------------------------------------------

;;; init.el ends here
