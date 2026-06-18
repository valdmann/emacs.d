;; -*- lexical-binding: t -*-

;; Usage:
;;   emacs --script use-package-report.el
;;
;; Optionally pass an init directory to report on:
;;   emacs --script use-package-report.el -- /path/to/emacs.d
;;
;; Prints the `use-package-report' buffer to stdout.  Statistics are
;; only collected when `use-package-compute-statistics' is non-nil at
;; load time, so we set it before loading init.

(require 'cl-lib)

(defun use-package-report--user-init-dir ()
  "Return the init directory to report on, from argv or default."
  (or (car command-line-args-left)
      user-emacs-directory))

(defun use-package-report--time-load (file)
  "Load FILE, printing any errors but continuing."
  (message "Loading %s..." file)
  (condition-case err
      (load file nil t)
    (error
     (princ (format "Error loading %s: %S\n" file err)
            'external-debugging-output))))

(defun use-package-report--print-report ()
  "Generate and print the `use-package-report' buffer to stdout."
  (let ((before (buffer-list)))
    (use-package-report)
    (let ((buf (car (cl-set-difference (buffer-list) before))))
      (if (not buf)
          (princ "No use-package report buffer was produced.\n")
        (with-current-buffer buf
          (princ (buffer-string)))
        (kill-buffer buf)))))

(defun use-package-report--run ()
  "Load init and print the `use-package' report."
  (let* ((init-dir (use-package-report--user-init-dir))
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
    ;; Statistics must be enabled before `use-package' forms are
    ;; evaluated during init.
    (setq use-package-compute-statistics t)
    (when (file-exists-p early-init)
      (use-package-report--time-load early-init))
    (when (file-exists-p init)
      (use-package-report--time-load init))
    (princ "\n=== use-package Report ===\n\n")
    (use-package-report--print-report)))

(use-package-report--run)

;;; use-package-report.el ends here
