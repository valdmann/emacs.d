;; -*- lexical-binding: t -*-

;; Usage:
;;   emacs --script profile-startup.el
;;
;; Optionally pass an init directory to profile:
;;   emacs --script profile-startup.el -- /path/to/emacs.d

(require 'profiler)
(require 'cl-lib)
(require 'rx)

(defun profile-startup--user-init-dir ()
  "Return the init directory to profile, from argv or default."
  (or (car command-line-args-left)
      user-emacs-directory))

(defvar profile-startup-min-samples 50
  "Drop report lines with fewer than this many samples.")

(defun profile-startup--filter-line (line)
  "Return LINE if it meets the sample threshold, else nil."
  (if (string-match (rx bos (zero-or-more space)
                        (group (one-or-more digit))
                        (one-or-more space)
                        (one-or-more digit) "%")
                  line)
      (let ((n (string-to-number (match-string 1 line))))
        (and (>= n profile-startup-min-samples) line))
    line))

(defun profile-startup--report ()
  "Print the filtered profiler report to stdout."
  (let ((before (buffer-list)))
    (profiler-report)
    (let ((buf (car (cl-set-difference (buffer-list) before))))
      (with-current-buffer buf
        (goto-char (point-min))
        (profiler-report-expand-entry t)
        (let ((lines (split-string (buffer-string) "\n"))
              (kept nil))
          (dolist (l lines)
            (let ((filtered (profile-startup--filter-line l)))
              (when filtered (push filtered kept))))
          (princ (mapconcat #'identity (nreverse kept) "\n"))))
      (kill-buffer buf))))

(defun profile-startup--time-load (file)
  "Load FILE, printing any errors but continuing."
  (message "Loading %s..." file)
  (condition-case err
      (load file nil t)
    (error
     (princ (format "Error loading %s: %S\n" file err)
            'external-debugging-output))))

(defun profile-startup--run ()
  "Profile startup and print a report."
  (let* ((init-dir (profile-startup--user-init-dir))
         (early-init (expand-file-name "early-init.el" init-dir))
         (init       (expand-file-name "init.el" init-dir)))
    ;; Ensure packages are discoverable so init.el doesn't error on
    ;; `use-package' / `require' forms.
    (setq package-user-dir (expand-file-name "elpa" init-dir))
    (package-initialize 'no-activate)
    (setq load-path (append (let ((default-directory package-user-dir))
                              (mapcar #'file-name-as-directory
                                      (file-expand-wildcards "*" t)))
                            load-path))
    (profiler-start 'cpu)
    (when (file-exists-p early-init)
      (profile-startup--time-load early-init))
    (when (file-exists-p init)
      (profile-startup--time-load init))
    (profiler-stop)
    (princ "\n=== Startup Profile ===\n\n")
    (profile-startup--report)))

(profile-startup--run)

;;; profile-startup.el ends here
