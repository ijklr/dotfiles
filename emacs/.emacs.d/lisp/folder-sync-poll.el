;;; folder-sync-poll.el --- Robust one-way polling sync via rsync  -*- lexical-binding: t; -*-
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: files, tools, convenience
;; URL: https://example.invalid/folder-sync-poll
;; SPDX-License-Identifier: MIT
;;
;; Summary:
;;   Periodically mirror a source directory into a destination using rsync.
;;   Non-overlapping runs; pending changes coalesce; structured logging.
;;
;; Usage:
;;   (require 'folder-sync-poll)
;;   (setq folder-sync-source "~/work/src/"
;;         folder-sync-dest   "~/work/backup/")
;;   (folder-sync-poll-mode 1)   ;; or M-x folder-sync-start

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(defgroup folder-sync-poll nil
  "Periodic one-way folder sync using rsync."
  :group 'tools
  :prefix "folder-sync-")

(defcustom folder-sync-source (expand-file-name "~/src/")
  "Source directory to mirror from (its *contents* will be copied)."
  :type 'directory)

(defcustom folder-sync-dest (expand-file-name "~/dst/")
  "Destination directory to mirror into (created if missing)."
  :type 'directory)

(defcustom folder-sync-interval 5
  "Seconds between sync attempts when `folder-sync-poll-mode' is enabled."
  :type '(integer :tag "Seconds"))

(defcustom folder-sync-rsync-program "rsync"
  "Path to the rsync executable."
  :type 'file)

(defcustom folder-sync-delete-behavior 'immediate
  "How deletions are handled by rsync."
  :type '(choice (const :tag "Immediate (--delete)" immediate)
                 (const :tag "Delay until end (--delete-delay)" delay)
                 (const :tag "Do not delete" none)))

(defcustom folder-sync-excludes
  '(".git/" ".DS_Store" "*~" "#*#" ".#*" ".cache/" "node_modules/"
    "build/" "out/" "cmake-build*/" "CMakeFiles/" "CMakeCache.txt"
    "*.o" "*.obj" "*.a" "*.so" "*.dylib" "*.dll" "compile_commands.json")
  "Patterns to exclude. Each entry becomes an `--exclude=PATTERN' arg."
  :type '(repeat (string :tag "Exclude pattern")))

(defcustom folder-sync-extra-rsync-args
  '("--safe-links" "--partial" "--human-readable" "--info=stats2")
  "Additional rsync arguments appended after core flags."
  :type '(repeat string))

(defcustom folder-sync-dry-run nil
  "If non-nil, pass -n to rsync for a dry run."
  :type 'boolean)

(defcustom folder-sync-log-buffer "*folder-sync*"
  "Buffer name used for logging."
  :type 'string)

(defcustom folder-sync-verbose t
  "If non-nil, log rsync invocations and status."
  :type 'boolean)

(defvar folder-sync--timer nil)
(defvar folder-sync--proc  nil)
(defvar folder-sync--pending nil)
(defvar folder-sync--last-exit-status 0)

(defun folder-sync--norm (dir)
  "Return DIR as an absolute directory name with trailing slash."
  (file-name-as-directory (expand-file-name dir)))

(defun folder-sync--ensure-setup ()
  "Validate configuration and ensure destination exists.
Signals `user-error' on invalid setup."
  (unless (executable-find folder-sync-rsync-program)
    (user-error "rsync not found (folder-sync-rsync-program=%S)"
                folder-sync-rsync-program))
  (let ((src (folder-sync--norm folder-sync-source)))
    (unless (file-directory-p src)
      (user-error "Source directory does not exist: %s" src)))
  (unless (file-directory-p folder-sync-dest)
    (condition-case err
        (make-directory folder-sync-dest t)
      (error (user-error "Failed to create destination %s: %s"
                         folder-sync-dest (error-message-string err))))))

(defun folder-sync--delete-flags ()
  (pcase folder-sync-delete-behavior
    ('immediate '("--delete"))
    ('delay     '("--delete-delay"))
    ('none      nil)))

(defun folder-sync--rsync-args ()
  "Assemble rsync argument vector from user options."
  (let* ((src (folder-sync--norm folder-sync-source))
         (dst (folder-sync--norm folder-sync-dest))
         (base '("-a"))
         (del  (folder-sync--delete-flags))
         (dry  (and folder-sync-dry-run '("-n")))
         (ex   (mapcar (lambda (p) (concat "--exclude=" p)) folder-sync-excludes)))
    (append dry base del ex folder-sync-extra-rsync-args (list src dst))))

(defun folder-sync--logf (level fmt &rest args)
  "Append a log line at LEVEL to `folder-sync-log-buffer'."
  (when (or folder-sync-verbose (not (memq level '(:debug))))
    (with-current-buffer (get-buffer-create folder-sync-log-buffer)
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
      (insert (format "[%s] " (substring (symbol-name level) 1)))
      (insert (apply #'format fmt args))
      (insert "\n"))))

(defun folder-sync--start-pass ()
  "Start one rsync pass if none is currently running.
If a pass is running, mark `folder-sync--pending' and return."
  (cond
   ((and folder-sync--proc (process-live-p folder-sync--proc))
    (setq folder-sync--pending t)
    (folder-sync--logf :debug "Tick while running -> set pending"))
   (t
    (folder-sync--ensure-setup)
    (let* ((prog folder-sync-rsync-program)
           (args (folder-sync--rsync-args))
           (buf  (get-buffer-create folder-sync-log-buffer)))
      (folder-sync--logf :info "Running: %s %s"
                         prog (mapconcat #'shell-quote-argument args " "))
      (setq folder-sync--proc
            (apply #'start-process "folder-sync-rsync" buf prog args))
      (set-process-sentinel
       folder-sync--proc
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (setq folder-sync--last-exit-status (process-exit-status proc))
           (setq folder-sync--proc nil)
           (if (zerop folder-sync--last-exit-status)
               (folder-sync--logf :info "rsync finished OK")
             (folder-sync--logf :error "rsync failed (exit %d)"
                                folder-sync--last-exit-status))
           (when folder-sync--pending
             (setq folder-sync--pending nil)
             (folder-sync--logf :debug "Running pending pass immediately")
             (folder-sync--start-pass)))))))))

(defun folder-sync--tick ()
  "Timer callback to attempt a pass (no overlap)."
  (condition-case err
      (folder-sync--start-pass)
    (error
     (folder-sync--logf :error "Setup/run error: %s" (error-message-string err)))))

;;;###autoload
(defun folder-sync-start (&optional interval)
  "Start periodic syncing.
With optional INTERVAL (prefix arg or call), set the period in seconds."
  (interactive "P")
  (folder-sync-stop)
  (let* ((ival (or (and interval (prefix-numeric-value interval))
                   folder-sync-interval))
         (ival (max 1 ival))) ;; floor to ≥1s
    (setq folder-sync--timer (run-with-timer 0 ival #'folder-sync--tick))
    (add-hook 'kill-emacs-hook #'folder-sync-stop)
    (message "folder-sync: started (every %ds)%s"
             ival (if folder-sync-dry-run " [dry-run]" ""))))

;;;###autoload
(defun folder-sync-stop ()
  "Stop periodic syncing and any pending immediate reruns.
If an rsync is currently running, it is interrupted."
  (interactive)
  (when (timerp folder-sync--timer)
    (cancel-timer folder-sync--timer))
  (setq folder-sync--timer nil
        folder-sync--pending nil)
  (when (and folder-sync--proc (process-live-p folder-sync--proc))
    (interrupt-process folder-sync--proc))
  (setq folder-sync--proc nil)
  (remove-hook 'kill-emacs-hook #'folder-sync-stop)
  (message "folder-sync: stopped"))

;;;###autoload
(defun folder-sync-run-now ()
  "Run a sync pass immediately (respecting non-overlap)."
  (interactive)
  (folder-sync--start-pass)
  (message "folder-sync: triggered one pass%s"
           (if (and folder-sync--proc (process-live-p folder-sync--proc))
               "" " (queued)")))

;;;###autoload
(define-minor-mode folder-sync-poll-mode
  "Global mode: continuously mirror `folder-sync-source' ⇒ `folder-sync-dest'."
  :init-value nil
  :global t
  :lighter " Sync↦"
  (if folder-sync-poll-mode
      (folder-sync-start)
    (folder-sync-stop)))

(provide 'folder-sync-poll)
;;; folder-sync-poll.el ends here
