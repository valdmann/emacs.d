;; -*- lexical-binding: t -*-

(require 'dash)
(require 'transient)

(defconst revcap-image-extensions '("png" "webp"))
(defconst revcap-caption-extension "txt")
(defconst revcap-marked-file-name "MARKED")

(defconst revcap-image-regexp
  (rx "." (eval `(or ,@revcap-image-extensions)) string-end))

(defconst revcap-caption-regexp
  (rx "." (eval revcap-caption-extension) string-end))

(defvar revcap-directory nil
  "Directory of the current session.")

(defvar revcap-image nil
  "Path of the currently displayed image.")

(defvar revcap-index nil
  "Index of the currently displayed image.")

(defvar revcap-images nil
  "List of image files for the current session.")

(defvar revcap-marked nil
  "List of images marked by user.")

(defvar revcap-marked-dirty nil
  "Indicates the in-memory state may have diverged from disk.")

(defun revcap-reset ()
  "Reset global state."
  (setq revcap-directory nil
        revcap-image nil
        revcap-index nil
        revcap-images nil
        revcap-marked nil))

(defun revcap-set-image (image)
  "Set the current IMAGE and update the index."
  (setq revcap-image image)
  (setq revcap-index (-elem-index image revcap-images)))

(defun revcap-set-index (index)
  "Set the current INDEX and update the image."
  (setq revcap-index index)
  (setq revcap-image (nth index revcap-images)))

(defun revcap-caption ()
  "Return path to current caption file."
  (revcap-corresponding-caption-name revcap-image))

(defun revcap-marked-file-path ()
  "Return path to persisted marked images list."
  (concat revcap-directory revcap-marked-file-name))

(defun revcap-natural-lessp (a b)
  (let ((a-name (file-name-nondirectory a))
        (b-name (file-name-nondirectory b)))
    (string-version-lessp a-name b-name)))

(defun revcap-natural-sort (lst)
  (sort lst #'revcap-natural-lessp))

(defun revcap-natural-insert (elem lst)
  "Insert ELEM into LST keeping natural sort order."
  (let ((head lst)
        (prev nil))
    (while (and lst (revcap-natural-lessp (car lst) elem))
      (setq prev lst
            lst (cdr lst)))
    (if prev
        (progn
          (setcdr prev (cons elem lst))
          head)
      (cons elem head))))

(defun revcap-load-marked ()
  "Load the list of marked images from the MARKED file."
  (let ((marked-file (revcap-marked-file-path)))
    (when (file-exists-p marked-file)
      (with-temp-buffer
        (insert-file-contents marked-file)
        (let* ((lines (split-string (buffer-string) "\n" t))
               (lines-sorted (revcap-natural-sort lines))
               (marked (--map (concat revcap-directory it) lines-sorted)))
          (setq revcap-marked marked
                revcap-marked-dirty t))))))

(defun revcap-save-marked ()
  "Save the list of marked images to the MARKED file."
  (when revcap-marked-dirty
    (let ((marked-file (revcap-marked-file-path)))
      (if revcap-marked
          (with-temp-buffer
            (dolist (img revcap-marked)
              (insert (file-name-nondirectory img) "\n"))
            (write-region (point-min) (point-max) marked-file))
        (when (file-exists-p marked-file)
          (delete-file marked-file))))
    (setq revcap-marked-dirty nil)))

(defun revcap-scan (dir)
  "Return sorted list of images found in DIR."
  (let* ((imgs (directory-files dir t revcap-image-regexp))
         (imgs (--filter (file-exists-p (revcap-corresponding-caption-name it))
                         imgs)))
    (revcap-natural-sort imgs)))

(defun revcap-corresponding-image-candidates (file)
  (cond
   ((string-match revcap-image-regexp file)
    (list file))
   ((string-match revcap-caption-regexp file)
    (let ((base (file-name-sans-extension file)))
      (--map (concat base "." it)
             revcap-image-extensions)))))


(defun revcap-corresponding-image (file)
  (let ((candidates (revcap-corresponding-image-candidates file)))
    (-first #'file-exists-p candidates)))

(defun revcap-corresponding-caption-name (file)
  (concat (file-name-sans-extension file) "." revcap-caption-extension))

(defun revcap-show ()
  "Display the current image with caption."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (let ((large-file-warning-threshold nil))
    (find-file revcap-image)
    (image-transform-fit-to-window))
  (other-window 1)
  (let* ((caption-file (revcap-caption))
         (caption-buffer (get-file-buffer caption-file)))
    (when (and caption-buffer
               (not (buffer-modified-p caption-buffer)))
      (with-current-buffer caption-buffer
          (revert-buffer :ignore-auto :noconfirm)))
    (find-file caption-file))
  (message "Showing %s" (file-name-nondirectory revcap-image)))

(defun revcap-show-other (&rest args)
  (let* ((distance (plist-get args :distance))
         (marked (plist-get args :marked))
         (images (if marked revcap-marked revcap-images))
         (total (length images))
         (index (if marked
                    (or (-elem-index revcap-image images)
                        (--find-index (not (revcap-natural-lessp revcap-image it))
                                      images)
                        (and (> total 0) (1- total)))
                  revcap-index))
         (next-index (mod (+ index distance) total))
         (next-image (nth next-index images)))
    (when next-image
      (revcap-set-image next-image)
      (revcap-show))))

(defun revcap-show-next ()
  "Go to next image."
  (interactive)
  (revcap-show-other :distance +1))

(defun revcap-show-prev ()
  "Go to previous image."
  (interactive)
  (revcap-show-other :distance -1))

(defun revcap-show-next-marked ()
  "Go to the next marked image."
  (interactive)
  (revcap-show-other :distance +1 :marked t))

(defun revcap-show-prev-marked ()
  "Go to the previous marked image."
  (interactive)
  (revcap-show-other :distance -1 :marked t))

(defun revcap-jump ()
  "Jump to a specific image in the current session."
  (interactive)
  (let* ((choices (mapcar #'file-name-nondirectory revcap-images))
         (selection (completing-read "Jump to image: " choices nil t)))
    (when selection
      (revcap-set-image (concat revcap-directory selection))
      (revcap-show))))

(defun revcap-jump-marked ()
  "Jump to a specific marked image."
  (interactive)
  (if (null revcap-marked)
      (message "No marked images.")
    (let* ((choices (mapcar #'file-name-nondirectory revcap-marked))
           (selection (completing-read "Jump to marked image: " choices nil t)))
      (when selection
        (revcap-set-image (concat revcap-directory selection))
        (revcap-show)))))

(defun revcap-toggle-mark ()
  "Toggle the mark for the current image."
  (interactive)
  (let ((img revcap-image))
    (if (member img revcap-marked)
        (progn
          (setq revcap-marked (delete img revcap-marked))
          (message "Unmarked %s" (file-name-nondirectory img)))
      (setq revcap-marked (revcap-natural-insert img revcap-marked))
      (message "Marked %s" (file-name-nondirectory img)))))

(defun revcap-start (image &optional force)
  "Start new session if needed."
  (let ((dir (file-name-directory image)))
    (unless (and (string= dir revcap-directory)
                 (not force))
      (setq revcap-images (revcap-scan dir))
      (unless revcap-images
        (revcap-reset)
        (user-error "No images found in %s" dir))
      (setq revcap-directory dir)
      (revcap-load-marked))
    (revcap-set-image image)))

(defun revcap-refresh ()
  (interactive)
  (revcap-start revcap-image t))

(defun revcap-finish ()
  "Switch to caption."
  (interactive)
  (revcap-save-marked)
  (find-file (revcap-caption)))

(defun revcap-describe-image ()
  (format "At %s/%s"
          (propertize (number-to-string (1+ revcap-index))
                      'face 'transient-value)
          (propertize (number-to-string (length revcap-images))
                      'face 'transient-value)))

(defun revcap-describe-mark ()
  (let* ((index (-elem-index revcap-image revcap-marked))
         (index (if index (number-to-string (1+ index)) "?"))
         (total (number-to-string (length revcap-marked))))
    (format "At %s/%s"
            (propertize index 'face 'transient-value)
            (propertize total 'face 'transient-value))))

(defun revcap-decribe-mark-toggle ()
  (if (member revcap-images revcap-marked)
      "Unmark"
    "Mark"))

;;;###autoload
(transient-define-prefix review-captions ()
  "Transient menu for caption review."
  [["Images"
    (:info #'revcap-describe-image :format " %d")
    ("n" "Next" revcap-show-next :transient t)
    ("p" "Previous" revcap-show-prev :transient t)
    ("j" "Jump" revcap-jump :transient t)]
   ["Marked"
    (:info #'revcap-describe-mark :format " %d")
    ("m" "Mark" revcap-toggle-mark :transient t)
    ("N" "Next" revcap-show-next-marked :transient t)
    ("P" "Previous" revcap-show-prev-marked :transient t)
    ("J" "Jump" revcap-jump-marked :transient t)]
   ["State"
    ("g" "Rescan directory" revcap-refresh :transient t)
    ("RET" "Switch to caption" revcap-finish)]]

  (interactive)
  (let* ((current-file (buffer-file-name))
         (current-image (revcap-corresponding-image current-file)))
    (unless current-image
      (user-error "Must be called from a buffer visiting an image or caption file"))
    (revcap-start current-image)
    (revcap-show)
    (transient-setup 'review-captions)))

(provide 'review-captions)
