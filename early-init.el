;; -*- lexical-binding: t -*-

(setq load-prefer-newer t)

(require 'xdg)
(defvar user-config-directory user-emacs-directory)
(defvar user-cache-directory (expand-file-name "emacs/" (xdg-cache-home)))
(defvar user-data-directory (expand-file-name "emacs/" (xdg-data-home)))
(defun in-config-directory (name) (expand-file-name name user-config-directory))
(defun in-cache-directory (name) (expand-file-name name user-cache-directory))
(defun in-data-directory (name) (expand-file-name name user-data-directory))
(dolist (directory (list user-data-directory user-cache-directory))
  (unless (file-exists-p directory)
    (make-directory directory)))

(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path (in-cache-directory "eln-cache/")))

(add-to-list 'load-path (in-config-directory "lib/packed"))
(add-to-list 'load-path (in-config-directory "lib/auto-compile"))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

(setq package-enable-at-startup nil)
