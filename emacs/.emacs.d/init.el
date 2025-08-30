;;; init.el --- shauncheng
(when (string-match-p "google" (system-name))
  (require 'google)
  (require 'citc))


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
 

;;;; ---- Completion stack: Vertico + Orderless + Marginalia ----
(use-package vertico
  :ensure t
  :init (vertico-mode 1))
 

(use-package marginalia
  :after vertico
  :ensure t
  :init
  (marginalia-mode 1))
 

;;(use-package orderless
;;  :ensure t
;;  :init
;;  ;; Make completion styles fuzzy/subsequence-friendly
;;  (setq completion-styles '(orderless basic)
;;        completion-category-defaults nil
;;        completion-category-overrides '((file (styles basic partial-completion)))))
 

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
  :bind (("C-c a" . magit-status)))
 

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
(global-set-key (kbd "C-0") #'delete-window)        ; close current window
(global-set-key (kbd "C-`") #'delete-window)        ; close current window
(global-set-key (kbd "M-o") #'other-window)         ; go to other window
(global-set-key (kbd "C-c w") #'other-window)         ; go to other window
(global-set-key (kbd "C-c e") #'previous-buffer) 
(global-set-key (kbd "C-c d") #'next-buffer) 
 

 

;;(global-set-key (kbd "C-c c") #'window-configuration-to-register) 
;;(global-set-key (kbd "C-c v") #'jump-to-register) 
(global-set-key (kbd "C-c c") #'bookmark-set)
(global-set-key (kbd "C-c v") #'bookmark-jump)
 

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
(global-set-key (kbd "C-c q") #'dired)
(setq dired-mouse-drag-files t)
 

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
 

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
 

;; ---- The Vertico Completion Stack ----
;; Add history and sorting with Prescient
(use-package prescient
  :after vertico
  :config
  (prescient-persist-mode 1))
 

(use-package vertico-prescient
  :after (vertico prescient)
  :init
  (vertico-prescient-mode 1))
;; Set the number of completions to show
(setq vertico-count 15)
;; ---- END The Vertico Completion Stack ----
 

;;Reload this file
(defun reload-init-file ()
  "Reload ~/.emacs.d/init.el without restarting Emacs."
  (interactive)
  (load-file user-init-file))
(global-set-key (kbd "<f12>") #'reload-init-file)
 

 

(windmove-default-keybindings) ; Shift+arrow moves between windows
 

 

;; Make sure consult is installed & recentf-mode is enabled
(global-set-key (kbd "C-x C-r") #'consult-recent-file)   ;; replace vanilla recentf
(global-set-key (kbd "C-c s") #'swiper)   ;; search CAPS+s
 

(global-set-key (kbd "C-c g") #'keyboard-quit)  ;; same as ctrl+g  
(defun my-recent-file-toggle ()
  "Call `consult-recent-file' or quit if the minibuffer is active."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (consult-recent-file)))
(global-set-key (kbd "C-c r") #'my-recent-file-toggle)
 

 

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
        (call-interactively #'find-file))))))
;; Make the toggle for it
(defun my-project-find-file-toggle ()
  "Call `consult-buffer' or quit if the minibuffer is active."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (call-interactively #'my/find-file-smart)))
(global-set-key (kbd "C-c f") #'my-project-find-file-toggle)
 

(defun my-consult-buffer-toggle ()
  "Call `consult-buffer' or quit if the minibuffer is active."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (consult-buffer)) )
(global-set-key (kbd "C-c b") 'my-consult-buffer-toggle)
 

;; Keep layout as tabs
(tab-bar-mode 1)           ; enable workspace tabs
(global-set-key (kbd "C-<prior>") #'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-<next>") #'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-c t") #'tab-bar-new-tab)
(global-set-key (kbd "C-c x") #'tab-bar-close-tab)
 

(global-set-key (kbd "C-x C-b") 'ibuffer)
 

;; closes old buffers. Does this even work?
(require 'midnight)
(midnight-mode 1)
 

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
 '(menu-bar-mode nil)
 '(package-selected-packages nil)
 '(tab-bar-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight regular :height 180 :width normal)))))
