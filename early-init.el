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

;; Don't load outdated bytecode.
(setq load-prefer-newer t)

;; Disable slow resize when changing fonts.
(setq frame-inhibit-implied-resize t)

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
(xterm-mouse-mode 1)
(column-number-mode)

;; Use only the buffer name as the title of the frame.
(setq frame-title-format "%b")

;; Show empty scratch buffer on startup.
(setq inhibit-startup-screen t
      initial-scratch-message "")
(fset 'display-startup-echo-area-message 'ignore)

;; Yes/no -> Y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Suppress warnings from native compiler.
(setq native-comp-async-report-warnings-errors nil)

;; Don't let customize write to init.el!
(setq custom-file (in-config-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set up package manager.
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Install missing packages by default.
(require 'use-package)
(setq use-package-always-ensure t)

;; Enable general in init.el.
(use-package general
  :vc (:url "https://github.com/noctuid/general.el" :rev newest))

;; Compile to bytecode automatically.
(use-package auto-compile
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t
        auto-compile-source-recreate-deletes-dest t
        auto-compile-toggle-deletes-nonlib-dest t
        auto-compile-update-autoloads t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;; Load theme before GUI is shown.
(use-package doom-themes
  :vc (:url "https://github.com/doomemacs/themes" :rev newest)
  :config
  (require 'doom-themes-ext-visual-bell "extensions/doom-themes-ext-visual-bell.el")
  (require 'doom-themes-ext-org "extensions/doom-themes-ext-org.el")
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))
