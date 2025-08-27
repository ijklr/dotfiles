;;; init.el --- shauncheng


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
(menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)

;; Package bootstrap
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t
      use-package-always-defer t)  ;; lazy load by default

;; Theme: Modus Operandi (built-in, light and readable)
(use-package modus-themes
  :demand t
  :config (load-theme 'modus-operandi t))

;; Completion
(use-package vertico :init (vertico-mode))
(use-package orderless :custom (completion-styles '(orderless)))
(use-package consult)

;; Git on demand
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; LSP on demand
(use-package eglot
  :hook ((c-mode c++-mode python-mode) . eglot-ensure))

;; Report startup time
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d GCs."
		     (emacs-init-time) gcs-done)))
;;Enable desktop-save-mode
(desktop-save-mode 1)
(setq desktop-restore-frames t) ;; Restore window layout
(setq desktop-dirname "~/.emacs.d/") ;; Save desktop file here

;;Set up package management:
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(unless package-archive-contents
  (package-refresh-contents))


;; Enable Evil mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)

(global-set-key (kbd "C-1") #'delete-other-windows)  ; keep this window only
(global-set-key (kbd "C-2") #'split-window-below)    ; split horizontally
(global-set-key (kbd "C-3") #'split-window-right)    ; split vertically
(global-set-key (kbd "C-=") #'balance-windows)       ; balance all splits
(global-set-key (kbd "<f4>") #'delete-window)        ; close current window
(global-set-key (kbd "C-0") #'delete-window)        ; close current window
(global-set-key (kbd "M-o") #'other-window)         ; go to other window

;;; Window management: hjkl layers
;; ------------------------------------------------------------

;; Move focus with Meta-hjkl
(global-set-key (kbd "M-h") #'windmove-left)
(global-set-key (kbd "M-j") #'windmove-down)
(global-set-key (kbd "M-k") #'windmove-up)
(global-set-key (kbd "M-l") #'windmove-right)

;; Swap windows with Meta+Shift-hjkl (Emacs 27+ has window-swap-states)
(defun my/window-swap (dir)
  "Swap current window with the window in direction DIR."
  (let ((other (windmove-find-other-window dir)))
    (when other
      (window-swap-states (selected-window) other))))

(global-set-key (kbd "M-H") (lambda () (interactive) (my/window-swap 'left)))
(global-set-key (kbd "M-J") (lambda () (interactive) (my/window-swap 'down)))
(global-set-key (kbd "M-K") (lambda () (interactive) (my/window-swap 'up)))
(global-set-key (kbd "M-L") (lambda () (interactive) (my/window-swap 'right)))

(global-set-key (kbd "<f5>") #'compile)
(global-set-key (kbd "<f6>") #'recompile)
(global-set-key (kbd "<f9>") #'magit-status)

;;Reload this file
(defun reload-init-file ()
  "Reload ~/.emacs.d/init.el without restarting Emacs."
  (interactive)
  (load-file user-init-file))
(global-set-key (kbd "<f12>") #'reload-init-file)


(windmove-default-keybindings) ; Shift+arrow moves between windows


;; Make sure consult is installed & recentf-mode is enabled
(global-set-key (kbd "C-x C-r") #'consult-recent-file)   ;; replace vanilla recentf
(global-set-key (kbd "C-c r")   #'consult-recent-file)   ;; mnemonic: r = recent

;; Unbind M-r in Vertico’s keymap and bind to exit
;; So that we can bind M-r to the correct function
(with-eval-after-load 'vertico
  (define-key vertico-map (kbd "M-r") #'vertico-exit))
(defun my-recent-file-toggle ()
  "Call `consult-recent-file' or quit if the minibuffer is active."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (consult-recent-file)))
(global-set-key (kbd "M-r") #'my-recent-file-toggle)

(defun my-project-find-file-toggle ()
  "Call `consult-buffer' or quit if the minibuffer is active."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (project-find-file)))
(global-set-key (kbd "M-f") 'my-project-find-file-toggle)

(defun my-consult-buffer-toggle ()
  "Call `consult-buffer' or quit if the minibuffer is active."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (consult-buffer))	)
(global-set-key (kbd "M-b") 'my-consult-buffer-toggle)

;; Keep layout as tabs
(global-tab-line-mode 0)   ; kill per-window buffer tabs
(tab-bar-mode 1)           ; enable workspace tabs
(setq tab-bar-show 1)      ; hide when only one tab
(global-set-key (kbd "C-<prior>") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<next>") #'tab-bar-switch-to-next-tab)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Good for moving file between Dired buffers
(setq dired-dwim-target t)

(global-auto-revert-mode 1)      ; refresh file-visiting buffers automatically
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-check-vc-info t)

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
   '(embark-consult evil-collection evil-leader magit modus-themes
		    multiple-cursors orderless vertico vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

