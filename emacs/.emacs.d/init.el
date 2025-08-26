;;; init.el --- shauncheng

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
(use-package magit :commands (magit-status))

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
(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)
(setq evil-leader/leader "<SPC>")
(require 'evil) ;; Load evil after evil-leader
(evil-mode 1)

;; Keybindings
(define-key evil-normal-state-map (kbd "<SPC> f r") 'recentf) ;; Recent files
(define-key evil-normal-state-map (kbd "<SPC> w w") 'other-window)       ;; Cycle windows
(define-key evil-normal-state-map (kbd "<SPC> g s") 'magit-status)       ;; Magit status

(windmove-default-keybindings) ; Shift+arrow moves between windows

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(embark-consult evil-collection evil-leader magit modus-themes
		    multiple-cursors orderless vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
