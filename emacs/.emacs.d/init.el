;;; init.el --- shauncheng 2025
;; ----------------------------------------------------------------------------
;; 1. Package Management
;; ----------------------------------------------------------------------------
(setq package-enable-at-startup nil)

(when (boundp 'gnutls-algorithm-priority)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

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
  :ensure t
  :config
  (setq completion-styles '(orderless basic)))
(use-package consult
  :ensure t
  :bind (;; A recursive grep
         ("M-s M-g" . consult-grep)
         ;; Search for files names recursively
         ("M-s M-f" . consult-find)
         ;; Search through the outline (headings) of the file
         ("M-s M-o" . consult-outline)
         ;; Search the current buffer
         ("M-s M-l" . consult-line)
         ;; Switch to another buffer, or bookmarked file, or recently
         ;; opened file.
         ("M-s M-b" . consult-buffer)))
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
(recentf-mode 1)
(setq recentf-max-saved-items 1000)   ;; how many files to keep
(setq recentf-max-menu-items 50)

;; from chatgpt
(use-package prescient
  :ensure t
  :custom
  (prescient-save-file (locate-user-emacs-file "var/prescient-save.el"))
  :config
  (make-directory (file-name-directory prescient-save-file) t)
  (prescient-persist-mode 1))

(use-package vertico-prescient
  :ensure t
  :after (vertico prescient)
  :config
  (vertico-prescient-mode 1))

;; Configure Vertico to use prescient
(use-package vertico-prescient
  :ensure t
  :after (vertico prescient)
  :init (vertico-prescient-mode 1))


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
(tool-bar-mode -1)
(menu-bar-mode -1)
;;(scroll-bar-mode -1) ;; the terminal version complains
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
(global-auto-revert-mode 1)

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
(keymap-global-set "<f8>" 'clang-format-buffer)
(setq lsp-clients-clangd-args '("--fallback-style=none"))

;; Apply to all C-like modes (C, C++, Java, etc.)
(setq c-default-style "linux"        ;; base style
      c-basic-offset 4)              ;; indent width

;; Good for moving file between Dired buffers
(setq dired-dwim-target t)

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
(which-key-mode t)

;; Column number display
(column-number-mode 1)

;; Column 80 indicator
(setq-default fill-column 80)
(setq-default fill-column-indicator-character ?\u2502); Thin vertical bar

(set-face-attribute 'fill-column-indicator nil
                    :foreground "#f0ffff" ; azure1  (M-x list-colors-display)
                    :background "#f0ffff" ;
                    :weight 'light)

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


(defvar xah-open-file-at-cursor-pre-hook nil "Hook for `xah-open-file-at-cursor'.
Functions in the hook is called in order, each given the raw input text (path) as arg.
The first return non-nil, its value is given to `xah-open-file-at-cursor' as input. rest functions in hook is ignored.
This is useful for transforming certain url into file path. e.g. change
http://xahlee.info/emacs/index.html
to C:/Users/xah/web/xahlee_info/emacs/index.html
, so instead of opening in browser, it opens in emacs as file.")

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.

• If there is selection, use it for path.
• Path can be {relative, full path, URL}.
• If the path starts with 「https*://」, open the URL in browser.
• Path may have a trailing 「:‹n›」 that indicates line number, or 「:‹n›:‹m›」 with line and column number. If so, jump to that line number.

If path does not have a file extension, automatically try with .el for elisp files.

See also `xah-open-file-at-cursor-pre-hook'.

This command is similar to `find-file-at-point' but without prompting for confirmation.
http://www.google.com
URL `http://xahlee.info/emacs/emacs/emacs_open_file_path_fast.html'
Created: 2020-10-17
Version: 2024-05-20"
  (interactive)
  (let (xinput xinput2 xpath)
    (setq xinput (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (let ((xp0 (point)) xp1 xp2
                         (xpathStops "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                     (skip-chars-backward xpathStops)
                     (setq xp1 (point))
                     (goto-char xp0)
                     (skip-chars-forward xpathStops)
                     (setq xp2 (point))
                     (goto-char xp0)
                     (buffer-substring-no-properties xp1 xp2))))
    (setq xinput2 (if (> (length xah-open-file-at-cursor-pre-hook) 0)
                      (let ((xprehook (run-hook-with-args-until-success 'xah-open-file-at-cursor-pre-hook xinput)))
                        (if xprehook xprehook xinput))
                    xinput))

    (setq xpath
          (cond
           ((string-match "^file:///[A-Za-z]:/" xinput2) (substring xinput2 8))
           ((string-match "^file://[A-Za-z]:/" xinput2) (substring xinput2 7))
           (t xinput2)))

    (if (string-match-p "\\`https?://" xpath)
        (browse-url xpath)
      (let ((xpathNoQ
             (let ((xHasQuery (string-match "\?[a-z]+=" xpath)))
               (if xHasQuery
                   (substring xpath 0 xHasQuery)
                 xpath))))
        (cond
         ((string-match "#" xpathNoQ)
          (let ((xfpath (substring xpathNoQ 0 (match-beginning 0)))
                (xfractPart (substring xpathNoQ (1+ (match-beginning 0)))))
            (if (file-exists-p xfpath)
                (progn
                  (find-file xfpath)
                  (goto-char (point-min))
                  (search-forward xfractPart))
              (progn
                (message "File does not exist. Created at\n%s" xfpath)
                (find-file xfpath)))))
         ((string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\(:[0-9]+\\)?\\'" xpathNoQ)
          (let ((xfpath (match-string-no-properties 1 xpathNoQ))
                (xlineNum (string-to-number (match-string-no-properties 2 xpathNoQ))))
            (if (file-exists-p xfpath)
                (progn
                  (find-file xfpath)
                  (goto-char (point-min))
                  (forward-line (1- xlineNum)))
              (progn
                (message "File does not exist. Created at\n%s" xfpath)
                (find-file xfpath)))))
         ((file-exists-p xpathNoQ)
          (progn ; open f.ts instead of f.js
            (let ((xext (file-name-extension xpathNoQ))
                  (xfnamecore (file-name-sans-extension xpathNoQ)))
              (if (and (string-equal xext "js")
                       (file-exists-p (concat xfnamecore ".ts")))
                  (progn
                    (find-file (concat xfnamecore ".ts"))
                    (warn "Typescript file .ts exist, opening it"))

                (find-file xpathNoQ)))))
         ((file-exists-p (concat xpathNoQ ".el"))
          (find-file (concat xpathNoQ ".el")))
         (t (progn
              (message "File does not exist. Created at\n%s" xpathNoQ)
              (find-file xpathNoQ))))))))


;;Enable Clipboard Access: In iTerm2, go to iTerm2 > Preferences (or Settings), select the Selection tab. Check the box for "Applications in terminal may access clipboard".
;; need this for copy from terminal to local clipboard
(setq xterm-extra-capabilities '(setSelection))


;;TTY fixes
(define-key key-translation-map (kbd "<f9>") (kbd "M-x"))
;;(keymap-global-set "C-@" 'split-window-below)
(keymap-global-set "<z>" 'other-window)


;; Custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;; Load a local config if it exists (no error, no message).
(load (locate-user-emacs-file "local.el") t t)

