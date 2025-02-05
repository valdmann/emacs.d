;; -*- lexical-binding: t; no-byte-compile: t -*-

;; Disable GC until after init.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; Use XDG directories.
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
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache (in-cache-directory "eln-cache/")))
(setq auto-save-list-file-prefix (in-data-directory "auto-save-list/"))

;; Don't load outdated bytecode.
(setq load-prefer-newer t)

;; Compile to bytecode automatically.
(add-to-list 'load-path (in-config-directory "lib/compat"))
(add-to-list 'load-path (in-config-directory "lib/packed"))
(add-to-list 'load-path (in-config-directory "lib/auto-compile"))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Disable built-in package manager.
(setq package-archives nil)
(setq package-enable-at-startup nil)

;; Disable slow resize when changing fonts.
(setq frame-inhibit-implied-resize t)

;; No warnings.
(setq warning-minimum-level :error)

;; Configure GUI before GUI is shown.
(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (fullscreen . maximized)
        (undecorated . t)
        (drag-internal-border . 1)
        (internal-border-width . 5)
        (font . "Iosevka Light-12")))
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(pixel-scroll-precision-mode 1)

;; Load theme before GUI is shown.
(add-to-list 'load-path (in-config-directory "lib/doom-modeline"))
(add-to-list 'load-path (in-config-directory "lib/doom-themes"))
(add-to-list 'load-path (in-config-directory "lib/doom-themes/extensions"))
(require 'doom-themes)
(require 'doom-themes-ext-visual-bell)
(require 'doom-themes-ext-org)
(load-theme 'doom-one t)
(doom-themes-visual-bell-config)
(doom-themes-org-config)

;; Use only the buffer name as the title of the frame.
(setq frame-title-format "%b")

;; Show empty scratch buffer on startup.
(setq inhibit-startup-screen t
      initial-scratch-message "")
(fset 'display-startup-echo-area-message 'ignore)

;; Yes/no -> Y/n.
(fset 'yes-or-no-p 'y-or-n-p)

