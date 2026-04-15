;; -*- lexical-binding: t -*-

(require 'recentf)
(require 'project)

(defvar magit-display-buffer-function)

(defvar welcome-recent-projects-limit 5)
(defvar welcome-recent-files-limit 10)

(defvar-local welcome--padding-overlay nil)

(define-button-type 'welcome-entry
  'face nil
  'follow-link t)

(defun welcome--project-roots ()
  "Return known project roots, excluding ELPA package directories."
  (cl-remove-if (lambda (r) (string-match-p "/elpa/" r))
                (project-known-project-roots)))

(defun welcome--recent-projects ()
  "Return recent projects ordered by recency of file access.
Derived from `recentf-list' matched against known project roots."
  (let ((roots (welcome--project-roots))
        (projects nil))
    (dolist (f recentf-list)
      (let* ((abbrev (abbreviate-file-name f))
             (match (cl-find-if (lambda (r) (string-prefix-p r abbrev)) roots)))
        (when (and match (not (member match projects)))
          (push match projects))))
    (nreverse projects)))

(defun welcome--insert-project-entry (path)
  "Insert a project entry with the last directory component highlighted."
  (let* ((trimmed (directory-file-name path))
         (name (concat (file-name-nondirectory trimmed) "/"))
         (prefix (file-name-directory trimmed)))
    (insert "    ")
    (insert-text-button (concat (propertize prefix 'face 'shadow) name)
                        'type 'welcome-entry
                        'action (lambda (_)
                              (if (file-directory-p (expand-file-name ".git" path))
                                  (let ((magit-display-buffer-function
                                         #'magit-display-buffer-same-window-except-diff-v1))
                                    (magit-status path))
                                (dired path))))
    (insert "\n")))

(defun welcome--insert-file-entry (path project-roots)
  "Insert a file entry with dimmed prefix and highlighted filename.
If PATH is inside a known project, the project name is also highlighted."
  (let* ((abbrev (abbreviate-file-name path))
         (name (file-name-nondirectory abbrev))
         (dir (file-name-directory abbrev))
         (root (cl-find-if (lambda (r) (string-prefix-p r abbrev)) project-roots))
         (root-parent (and root (file-name-directory (directory-file-name root))))
         (project-name (and root (file-name-nondirectory (directory-file-name root))))
         (rest-of-dir (and root (string-remove-prefix root dir))))
    (insert "    ")
    (let ((label (if root
                     (concat (propertize root-parent 'face 'shadow)
                             project-name
                             (propertize (concat "/" rest-of-dir) 'face 'shadow)
                             name)
                   (concat (propertize dir 'face 'shadow) name))))
      (insert-text-button label
                          'type 'welcome-entry
                          'action (lambda (_) (find-file path))))
    (insert "\n")))

(defun welcome--max-line-width (win)
  "Return the pixel width of the widest line in the current buffer.
Measurements are taken using window WIN."
  (let ((max-w 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq max-w (max max-w (car (window-text-pixel-size win
                                      (line-beginning-position)
                                      (line-end-position)))))
        (forward-line 1)))
    max-w))

(defun welcome--recenter ()
  "Center the welcome buffer content in its window."
  (when-let* ((buf (get-buffer "*welcome*"))
              (win (get-buffer-window buf))
              (ov (buffer-local-value 'welcome--padding-overlay buf)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        ;; Reset centering
        (overlay-put ov 'before-string nil)
        (remove-text-properties (point-min) (point-max)
                                '(line-prefix nil wrap-prefix nil))
        ;; Measure raw content in pixels
        (let* ((content-h (cdr (window-text-pixel-size win)))
               (vpad-px (max 0 (/ (* (- (window-body-height win t) content-h) 3) 10)))
               (vpad-lines (/ vpad-px (default-line-height)))
               (half-w (/ (welcome--max-line-width win) 2))
               (prefix (propertize " " 'display
                                   `(space :align-to (- center (,half-w))))))
          (overlay-put ov 'before-string (make-string vpad-lines ?\n))
          (put-text-property (point-min) (point-max) 'line-prefix prefix)
          (put-text-property (point-min) (point-max) 'wrap-prefix prefix))))))

(defun welcome--on-frame-size-change (frame)
  "Re-center welcome buffer when FRAME is resized."
  (when (get-buffer-window "*welcome*" frame)
    (welcome--recenter)))

(define-derived-mode welcome-mode special-mode "Welcome"
  "Major mode for the welcome screen."
  (setq-local global-hl-line-mode nil)
  (setq-local evil-normal-state-cursor '(nil))
  (setq welcome--padding-overlay (make-overlay (point-min) (point-min)))
  (add-hook 'window-size-change-functions #'welcome--on-frame-size-change)
  (add-hook 'kill-buffer-hook
            (lambda ()
              (remove-hook 'window-size-change-functions
                           #'welcome--on-frame-size-change))
            nil t))

(defun welcome-buffer ()
  "Create and return the *welcome* buffer."
  (let ((buf (get-buffer-create "*welcome*")))
    (with-current-buffer buf
      (when welcome--padding-overlay
        (delete-overlay welcome--padding-overlay))
      (let ((inhibit-read-only t)
            (project-roots (welcome--project-roots)))
        (erase-buffer)
        (insert (propertize (format "Emacs %s" emacs-version)
                            'face '(:height 1.5 :weight bold))
                "\n\n")
        (insert "Recent Projects:\n")
        (dolist (p (seq-take (welcome--recent-projects)
                             welcome-recent-projects-limit))
          (welcome--insert-project-entry p))
        (insert "\nRecent Files:\n")
        (dolist (f (seq-take recentf-list welcome-recent-files-limit))
          (welcome--insert-file-entry f project-roots))
        (goto-char (point-min)))
      (welcome-mode)
      (run-with-timer 0 nil #'welcome--recenter))
    buf))

(provide 'welcome)
