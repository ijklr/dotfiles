;; my-find-file-smart.el --- Simple project-aware file opener -*- lexical-binding: t; -*-
;;
;; Author: Shaun Cheng
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: files, convenience, project
;; URL: https://github.com/ijklr/dotfiles
;;
;;; Commentary:
;; One command:

(require 'project) ;; built-in

;;;###autoload

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

(provide 'my-find-file-smart)
;;; my-find-file-smart.el ends here
