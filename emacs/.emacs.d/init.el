;;; init.el --- shauncheng 2025
;; ----------------------------------------------------------------------------
;; 1. Package Management
;; ----------------------------------------------------------------------------
(setq package-enable-at-startup nil)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (message "Refreshing package archives...")
  (condition-case err
      (package-refresh-contents)
    (error (message "Package refresh failed: %s." err))))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ;; ----------------------------------------------------------------------------
;; ;; 2. Modern Completion System: Vertico, Consult, Embark, Orderless, etc.
;; ;; From https://protesilaos.com/codelog/2024-02-17-emacs-modern-minibuffer-packages/
;; ;; ----------------------------------------------------------------------------
(use-package vertico
  :ensure t
  :config
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (vertico-mode 1))
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package prescient :config (prescient-persist-mode 1))
(use-package vertico-prescient
  :after (vertico prescient)
  :config
  ;; Only use prescient for sorting, not filtering:
  (setq vertico-prescient-enable-filtering nil
        vertico-prescient-enable-sorting t)
  (vertico-prescient-mode 1))

(use-package consult
  :bind (("M-s M-r" . consult-ripgrep) ;; Use consult-ripgrep by default for big searches
         ("M-s M-g" . consult-grep)
         ("M-s M-l" . consult-line)
         ("M-s M-f" . consult-find)
         ("M-s M-o" . consult-outline)
         ("M-s M-b" . consult-buffer)))
;; Optional: tweak ripgrep args
(setq consult-ripgrep-args
      "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number --hidden -g !.git/")

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package embark-consult
  :ensure t)
(use-package wgrep
  :ensure t
  :bind ( :map grep-mode-map
          ("e" . wgrep-change-to-wgrep-mode)
          ("C-x C-q" . wgrep-change-to-wgrep-mode)
          ("C-c C-c" . wgrep-finish-edit)))
(savehist-mode 1)
(setq history-length 5000
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring))

;;Recentf file: ensure directory exists to avoid startup warnings:
(setq recentf-save-file (locate-user-emacs-file "var/recentf"))
(make-directory (file-name-directory recentf-save-file) t)
(recentf-mode 1)
(setq recentf-max-saved-items 1000)   ;; how many files to keep
(setq recentf-max-menu-items 50)

;;Save place (cursor position across files):
(save-place-mode 1)


;; from chatgpt
(use-package prescient
  :ensure t
  :custom
  (prescient-save-file (locate-user-emacs-file "var/prescient-save.el"))
  :config
  (make-directory (file-name-directory prescient-save-file) t)
  (prescient-persist-mode 1))

;; ----------------------------------------------------------------------------
;; 3. Evil Mode (Vim keybindings)
;; ----------------------------------------------------------------------------
(setq evil-want-C-u-scroll t) ;; This must be BEFORE (use-package evil).
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo) ;; so that ctrl+r redo works
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "C-g") 'keyboard-quit)
  (define-key evil-visual-state-map (kbd "C-g") 'keyboard-quit))

(use-package evil-collection
  :after (evil consult)
  :config
  (evil-collection-init))


;; Install + wire up evil-nerd-commenter
(use-package evil-nerd-commenter
  :after evil
  :bind (("<f12>" . evilnc-comment-or-uncomment-lines))  ;; VSCode/JetBrains-style toggle
  :config
  ;; Vim-like keys: `gcc` (line), `gc{motion}` (operator), Visual `gc`
  (evil-define-key 'normal prog-mode-map (kbd "gc")  #'evilnc-comment-operator)
  (evil-define-key 'visual prog-mode-map (kbd "gc")  #'evilnc-comment-operator)
  (evil-define-key 'normal prog-mode-map (kbd "gcc") #'evilnc-comment-or-uncomment-lines))


;; ----------------------------------------------------------------------------
;; 4. Basic Emacs Quality of Life Improvements
;; ----------------------------------------------------------------------------
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(global-display-line-numbers-mode 1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      create-lockfiles nil)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq use-dialog-box nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 4 1024 1024))
(setq frame-title-format
      '("" invocation-name ": "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(show-paren-mode 1)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(column-number-mode 1)

;; (eval-when-compile (require 'use-package))

;; M-x must go through TTY
;;(keymap-set global-map "<f9>" #'consult-M-x)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))) ; Store backups in ~/.emacs.d/backups/
(setq backup-by-copying t) ; Copy files for backups

;; Make sure M-. / M-, are xref go-to-definition / pop-back
(keymap-global-set "M-." 'xref-find-definitions)
(keymap-global-set "M-," 'xref-pop-marker-stack)
;; Ensure these are active in normal state too:
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "M-.") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "M-,") #'xref-pop-marker-stack))

(defun my/enable-line-numbers-for-code ()
  "Enable line numbers for programming modes."
  (when (derived-mode-p 'prog-mode)
    (display-line-numbers-mode 1)))
(add-hook 'prog-mode-hook 'my/enable-line-numbers-for-code)

;; Minimal UI
(setq inhibit-startup-message t
      initial-scratch-message nil
      ring-bell-function 'ignore)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Theme: Modus Operandi
(load-theme 'modus-operandi t)

;; Git on demand
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package eglot
  ;; If you're on Emacs 29+, eglot is built-in, so :ensure is not needed.
  ;; But having it is harmless and good for compatibility with older versions.
  :ensure t
  :hook ((c-mode . eglot-ensure)
         (c++-mode . eglot-ensure)))

(with-eval-after-load 'eglot
  (setf (alist-get 'c++-mode eglot-server-programs)
        '("clangd" "--fallback-style=none"))
  (setf (alist-get 'c-mode eglot-server-programs)
        '("clangd" "--fallback-style=none")))


;; Report startup time
(add-hook 'emacs-startup-hook
	      (lambda ()
	        (message "Emacs ready in %s with %d GCs."
		             (emacs-init-time) gcs-done)))
;;Enable desktop-save-mode
(desktop-save-mode 1)
(setq desktop-restore-frames t) ;; Restore window layout
(setq desktop-dirname "~/.emacs.d/") ;; Save desktop file here

(keymap-global-set "C-1" 'delete-other-windows)
(keymap-global-set "C-2" 'split-window-below)
(keymap-global-set "C-3" 'split-window-right)
(keymap-global-set "C-=" 'balance-windows)
;; TTY fix
(keymap-global-set "<f1>" 'delete-other-windows)
(keymap-global-set "<f2>" 'split-window-below)
(keymap-global-set "<f3>" 'split-window-right)
(keymap-global-set "<f4>" 'delete-window)

;; Keep layout as tabs
(tab-bar-mode 1)           ; enable workspace tabs
(keymap-global-set "C-<prior>" 'tab-bar-switch-to-prev-tab)
(keymap-global-set "C-<next>" 'tab-bar-switch-to-next-tab)
;;Tab bar: add numeric shortcuts
(dotimes (i 9)
  (keymap-global-set (format "M-%d" (1+ i))
                     (lambda () (interactive) (tab-bar-select-tab (1+ i)))))

;; Define a custom keymap for M-o
(defvar my-custom-keymap (make-sparse-keymap)
  "My custom keymap for M-o prefix shortcuts.")
(keymap-global-set "M-o" my-custom-keymap)
;;TTY fix
(keymap-global-set "ø" my-custom-keymap)

;; Define shortcuts under M-o
(keymap-set my-custom-keymap "d" 'next-buffer)
(keymap-set my-custom-keymap "c" 'bookmark-set)
(keymap-set my-custom-keymap "v" 'bookmark-jump)
(keymap-set my-custom-keymap "t" 'tab-new)
(keymap-set my-custom-keymap "a" 'magit-status)
(keymap-set my-custom-keymap "w" 'tab-close)
(keymap-set my-custom-keymap "q" 'delete-window)
(keymap-set my-custom-keymap "e" 'previous-buffer)
(keymap-set my-custom-keymap "s" 'consult-line)
(keymap-set my-custom-keymap "x" 'kill-this-buffer)

;; Make sure consult is installed & recentf-mode is enabled
(keymap-global-set "C-x C-b" 'ibuffer)
(setq ibuffer-expert t) ; fewer prompts


;; Move focus with Meta-hjkl
(keymap-global-set "M-h" 'windmove-left)
(keymap-global-set "M-j" 'windmove-down)
(keymap-global-set "M-k" 'windmove-up)
(keymap-global-set "M-l" 'windmove-right)
(keymap-global-set "C-c h" 'windmove-left)
(keymap-global-set "C-c j" 'windmove-down)
(keymap-global-set "C-c k" 'windmove-up)
(keymap-global-set "C-c l" 'windmove-right)
(windmove-default-keybindings) ; Shift+arrow moves between windows

;; Swap windows with Meta+Shift-hjkl (Emacs 27+ has window-swap-states)
(defun my/window-swap (dir)
  "Swap current window with the window in direction DIR."
  (let ((other (windmove-find-other-window dir)))
    (when other
      (window-swap-states (selected-window) other))))
(keymap-global-set "M-H" (lambda () (interactive) (my/window-swap 'left)))
(keymap-global-set "M-J" (lambda () (interactive) (my/window-swap 'down)))
(keymap-global-set "M-K" (lambda () (interactive) (my/window-swap 'up)))
(keymap-global-set "M-L" (lambda () (interactive) (my/window-swap 'right)))

;; quick “toggle to last buffer” (very popular)
(defun sc/alternate-buffer () (interactive)
       (switch-to-buffer (other-buffer (current-buffer) t)))
(keymap-set my-custom-keymap "z" 'sc/alternate-buffer)

(keymap-global-set "<f5>" 'compile)
;;(keymap-global-set "<f6>" 'recompile)
;; F6: run ./a.out
(defun my-run-aout ()
  "Run ./a.out asynchronously."
  (interactive)
  (async-shell-command "./a.out"))
(keymap-global-set "<f6>" 'my-run-aout)

(setq dired-mouse-drag-files t)

;;Reload this file
(defun reload-init-file ()
  "Reload ~/.emacs.d/init.el without restarting Emacs."
  (interactive)
  (load-file user-init-file))

(defun my-recent-file-toggle ()
  "Call `consult-recent-file' or quit if the minibuffer is active."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (consult-recent-file)))
(keymap-set my-custom-keymap "r" 'my-recent-file-toggle)

;;   Helper function to for find file:
;;   - If inside a project → `project-find-file` (fast, respects VCS ignores).
;;   - Otherwise → `find-file` from the project root (if any) or current dir.
(defun my/find-file-smart (&optional prompt-directory)
  "Open files with a simple project-aware behavior.
No prefix: If in a project, use `project-find-file`. Otherwise, use `find-file`
from the project root (if detectable) or the current directory.
With C-u (PROMPT-DIRECTORY non-nil): Prompt for a directory and then run
`find-file` rooted there."
  (interactive "P")
  (let* ((proj (project-current))
	     (root (and proj (ignore-errors (project-root proj)))))
    (cond
     ;; In a project and no explicit prompt: use project finder.
     ((and proj (not prompt-directory))
      (call-interactively #'project-find-file))
     ;; Otherwise: classic find-file, rooted at chosen dir / project root / cwd.
     (t
      (let* ((base (or root default-directory))
	         (dir  (if prompt-directory
		               (read-directory-name "Find file in: " base nil t)
		             base))
	         (default-directory dir))
	    (call-interactively #'consult-find))))))
;; Make the toggle for it
(defun my-project-find-file-toggle ()
  "Call `consult-buffer' or quit if the minibuffer is active."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (call-interactively #'my/find-file-smart)))
(keymap-set my-custom-keymap "f" 'my-project-find-file-toggle)

(defun my-consult-buffer-toggle ()
  "Call `consult-buffer' or quit if the minibuffer is active."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (consult-buffer)) )
(keymap-set my-custom-keymap "b" 'my-consult-buffer-toggle)

(defun indent-whole-buffer ()
  "Indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))
(use-package clang-format :ensure t)
(keymap-global-set "<f8>" 'clang-format-buffer)
(setq lsp-clients-clangd-args '("--fallback-style=none"))

;; Apply to all C-like modes (C, C++, Java, etc.)
(setq c-default-style "linux"        ;; base style
      c-basic-offset 4)              ;; indent width

;; Good for moving file between Dired buffers
(setq dired-dwim-target t)

;;Dired to auto-refresh when files change
(add-hook 'dired-mode-hook 'auto-revert-mode)

(setq global-auto-revert-non-file-buffers t) ;; auto-refresh dired etc.
(setq auto-revert-verbose nil) ;;reduce mini-buffer noise
(global-auto-revert-mode 1)   ;; refresh file-visiting buffers automatically
;; REMINDER: DO NOT ENABLE THIS. IT DEFAULTS TO NIL ALREADY:
;;(setq auto-revert-check-vc-info t) ;;THIS IS LAGGY AF!


;; highlight diff in realtime
(use-package diff-hl
  :ensure t
  :init
  ;; Enable everywhere (file-visiting buffers).
  (global-diff-hl-mode 1)
  :hook
  ;; Show VCS changes in Dired buffers.
  (dired-mode . diff-hl-dired-mode)
  ;; Keep indicators correct around Magit refreshes.
  (magit-pre-refresh  . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  ;; Update diffs on-the-fly as you edit (instead of only on save).
  (diff-hl-flydiff-mode 1)

  ;; In terminals without fringes, use the margin.
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1)))

(use-package vdiff
  :ensure t
  :defer t  ; Only load when actually needed
  :commands (vdiff-buffers vdiff-quit))
(defun my-vdiff-with-origin-master ()
  "Compare the current buffer with its origin/master version using vdiff."
  (interactive)
  (let* ((file (buffer-file-name))
         (rel-file (file-relative-name file (vc-root-dir)))
         (temp-file (make-temp-file "origin-master-"))
         (temp-buffer (get-buffer-create "*origin/master*")))
    (shell-command (format "git show origin/master:%s > %s" rel-file temp-file))
    (with-current-buffer temp-buffer
      (insert-file-contents temp-file)
      (set-buffer-modified-p nil))
    (vdiff-buffers (current-buffer) temp-buffer)
    (delete-file temp-file)))

(defun my-vdiff-toggle-or-quit ()
  "Toggle vdiff with origin/master or quit if active."
  (interactive)
  (if (get-buffer "*origin/master*")
      (progn (message "hello I am quitting vdiff!")
	         (vdiff-quit)
	         (kill-buffer "*origin/master*")
	         (delete-other-windows))
    (progn (message "opening vdiff!")
	       (my-vdiff-with-origin-master))
    )
  )
(keymap-global-set "<f7>" 'my-vdiff-toggle-or-quit)

;;(use-package vterm :ensure t)
;; (use-package dirvish :init (dirvish-override-dired-mode 1))

;; Popup for easy discoverability
(use-package which-key :ensure t :config (which-key-mode 1))

;; Column number display
(column-number-mode 1)

;; Column 80 indicator
(setq-default fill-column 80)
(setq-default fill-column-indicator-character ?\u2502); Thin vertical bar

(set-face-attribute 'fill-column-indicator nil
                    :foreground "#f0ffff" ; azure1  (M-x list-colors-display)
                    :background "#f0ffff" ;
                    :weight 'light)

;; Display FCI globally, let theme style it
(global-display-fill-column-indicator-mode 1)

;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#d7ffff") ; Replace with your desired color code

;;enable mouse in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

;; --- Doom Modeline Setup ---
(use-package all-the-icons
  :ensure t
  ;; Install fonts once with: M-x all-the-icons-install-fonts
  )

(use-package mood-line
  :ensure t
  :init (mood-line-mode 1))
;; Optional for icons in some segments:
;; (use-package nerd-icons :ensure t)

;;Enable Clipboard Access: In iTerm2, go to iTerm2 > Preferences (or Settings), select the Selection tab. Check the box for "Applications in terminal may access clipboard".
;; need this for copy from terminal to local clipboard
(setq xterm-extra-capabilities '(setSelection))

;;TTY fixes
(define-key key-translation-map (kbd "<f9>") (kbd "M-x"))

;; Custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;; Load a local config if it exists (no error, no message).
(load (locate-user-emacs-file "local.el") t t)

;; Enable word wrap
(global-visual-line-mode 1)

;; My SPC leader
(use-package general
  :config
  (general-create-definer spc-leader
    :states '(normal visual emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  (spc-leader
    "a" '(artist-mode :which-key "toggle artist-mode")))

(defun insert-epoch-time ()
  "Insert the current epoch time (seconds since 1970-01-01) at point."
  (interactive)
  (insert (format-time-string "%s")))


;; GC after startup
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))
