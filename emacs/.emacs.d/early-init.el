;;; early-init.el --- minimal speedups
(setq package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      frame-inhibit-implied-resize t)
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 64000000   ;; 64 MB as integer
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/file-name-handler-alist)))
;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Defer resize handling
(setq frame-inhibit-implied-resize t)
;;; early-init.el ends here


