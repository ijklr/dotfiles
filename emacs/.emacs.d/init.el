;;; init.el --- shauncheng 2025
;; Load a local config if it exists (no error, no message).
(load (locate-user-emacs-file "local.el") t t)

;; --- Package Management (use-package) ---
(require 'package)
;; Add MELPA in addition to the default GNU ELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; Configure use-package defaults
(setq use-package-always-ensure t   ;; Install packages if not present
      use-package-always-defer t)   ;; Defer loading for better startup time

;; Backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))) ; Store backups in ~/.emacs.d/backups/
(setq backup-by-copying t) ; Copy files for backups

;; Make sure M-. / M-, are xref go-to-definition / pop-back
(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "M-,") #'xref-pop-marker-stack)
;; Ensure these are active in normal state too:
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "M-.") #'xref-find-definitions)
  (define-key evil-normal-state-map (kbd "M-,") #'xref-pop-marker-stack))

(recentf-mode 1)                     ;; turn it on
(setq recentf-max-saved-items 1000)   ;; how many files to keep
(setq recentf-max-menu-items 50)

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

;; Theme: Modus Operandi (built-in, light and readable)
(use-package modus-themes
  :demand t
  :config (load-theme 'modus-operandi t))


;;;; ---- Completion stack: Vertico + Orderless + Marginalia ----
(use-package vertico
  :ensure t
  :init (vertico-mode 1))

(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode 1))

(use-package orderless
  :ensure t
  :init
  ;; This is the magic line that enables the fuzzy matching
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))


;;;; ---- Consult (find, ripgrep, xref, etc.) ----
(use-package consult
  :ensure t
  :init
  ;; Make async sources snappy; tune if you type slowly/quickly
  (setq consult-async-min-input 2
        consult-async-refresh-delay 0.08))


;; Git on demand
(use-package magit
  :ensure t
  :bind (("M-o a" . magit-status)))

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

;; Enable Evil mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

;; Install + wire up evil-nerd-commenter
(use-package evil-nerd-commenter
  :after evil
  :bind (("C-/" . evilnc-comment-or-uncomment-lines))  ;; VSCode/JetBrains-style toggle
  :config
  ;; Vim-like keys: `gcc` (line), `gc{motion}` (operator), Visual `gc`
  (evil-define-key 'normal prog-mode-map (kbd "gc")  #'evilnc-comment-operator)
  (evil-define-key 'visual prog-mode-map (kbd "gc")  #'evilnc-comment-operator)
  (evil-define-key 'normal prog-mode-map (kbd "gcc") #'evilnc-comment-or-uncomment-lines))

(keymap-global-set "C-1" 'delete-other-windows)
(keymap-global-set "C-2" 'split-window-below)
(keymap-global-set "C-3" 'split-window-right)
(keymap-global-set "C-=" 'balance-windows)
(keymap-global-set "C-`" 'delete-windows)

;; Keep layout as tabs
(tab-bar-mode 1)           ; enable workspace tabs
(global-set-key (kbd "C-<prior>") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<next>") #'tab-bar-switch-to-next-tab)

;; Define a custom keymap for M-o
(defvar my-custom-keymap (make-sparse-keymap)
  "My custom keymap for M-o prefix shortcuts.")
(keymap-global-set "M-o" my-custom-keymap)

;; Define shortcuts under M-o
(keymap-set my-custom-keymap "d" 'next-buffer)
(keymap-set my-custom-keymap "c" 'bookmark-set)
(keymap-set my-custom-keymap "v" 'bookmark-jump)
(keymap-set my-custom-keymap "t" 'tab-new)
(keymap-set my-custom-keymap "q" 'dired)
(keymap-set my-custom-keymap "w" 'tab-close)
(keymap-set my-custom-keymap "e" 'previous-buffer) 
(keymap-set my-custom-keymap "s" 'swiper) 
(keymap-set my-custom-keymap "x" 'kill-this-buffer)

;; Make sure consult is installed & recentf-mode is enabled
(keymap-global-set "C-x C-r" 'consult-recent-file)   ;; replace vanilla recentf
(keymap-global-set "C-x C-b" 'ibuffer)

;; Move focus with Meta-hjkl
(keymap-global-set "M-h" 'windmove-left)
(keymap-global-set "M-j" 'windmove-down)
(keymap-global-set "M-k" 'windmove-up)
(keymap-global-set "M-l" 'windmove-right)
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
(keymap-global-set "<f6>" 'recompile)
(keymap-global-set "<f9>" 'magit-status)
(setq dired-mouse-drag-files t)

;; History for M-x
(use-package savehist :init (savehist-mode 1)) ; keep history for Orderless/Minibuffer

;;Reload this file
(defun reload-init-file ()
  "Reload ~/.emacs.d/init.el without restarting Emacs."
  (interactive)
  (load-file user-init-file))
(keymap-global-set "<f12>" 'reload-init-file)

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
(keymap-global-set "<f8>" 'indent-whole-buffer)

;; closes old buffers. Does this even work?
(require 'midnight)
(midnight-mode 1)

;; Good for moving file between Dired buffers
(setq dired-dwim-target t)

(global-auto-revert-mode 1)      ; refresh file-visiting buffers automatically
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-check-vc-info t)

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

(use-package vterm :ensure t)
(use-package dirvish :init (dirvish-override-dired-mode 1))

;; Popup for easy discoverability
(which-key-mode t)

;; Column number display
(column-number-mode 1)

;; Column 80 indicator
(setq-default fill-column 80)
(keymap-set my-custom-keymap "l" 'display-fill-column-indicator-mode)

(setq-default fill-column-indicator-character ?\u2502) ; Thin vertical bar

(set-face-attribute 'fill-column-indicator nil
                    :foreground "#f0ffff" ; azure1  (M-x list-colors-display)
                    :background "#f0ffff" ;
                    :weight 'light)


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a9028cd93db14a5d6cdadba789563cb90a97899c4da7df6f51d58bb390e54031"
     "7235b77f371f46cbfae9271dce65f5017b61ec1c8687a90ff30c6db281bfd6b7"
     default))
 '(package-selected-packages
   '(centaur-tabs consult counsel deadgrep diff-hl dirvish evil
		  evil-nerd-commenter helm lsp-mode magit marginalia
		  modus-themes orderless projectile ripgrep vterm)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Noto Sans Mono" :foundry "GOOG" :slant normal :weight regular :height 143 :width normal)))))

