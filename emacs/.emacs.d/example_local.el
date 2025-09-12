;; This file is just an example of what local.el can look like

;; my folder sync
(add-to-list 'load-path "~/.emacs.d/lisp")  ;; wherever you saved the file
(require 'folder-sync-poll)

(setq folder-sync-source "~/experimental/"
      folder-sync-dest   "~/test/"
      ;;folder-sync-dest   "/google/src/cloud/shauncheng/emacs/google3/experimental/users/shauncheng/"
      folder-sync-interval 5
      folder-sync-delete-behavior 'none ;; do not delete destination files if local ones are deleted
      folder-sync-dry-run nil)               ;; verify once, then set to nil
(folder-sync-poll-mode 1)
