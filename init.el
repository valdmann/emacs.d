;; -*- lexical-binding: t -*-

(define-prefix-command 'spc-map)
(define-prefix-command 'spc-lsp-map)
(define-prefix-command 'spc-org-map)
(keymap-set spc-map "l" '("lsp" . spc-lsp-map))
(keymap-set spc-map "o" '("org" . spc-org-map))

(defun jv/buffer-path ()
  (or (buffer-file-name) (user-error "Buffer is not visiting a file")))

(defun jv/project-buffer-path ()
  (file-relative-name (jv/buffer-path)
                      (or (project-root (project-current))
                          (user-error "Not in a project"))))

(defun jv/kill (str)
  (kill-new str) (message "%s" str))

(defun jv/kill-location (file)
  (jv/kill (if-let* (((use-region-p))
                     (beg (line-number-at-pos (region-beginning)))
                     (end (line-number-at-pos (region-end)))
                     ((< beg end)))
               (format "%s:%d-%d" file beg end)
             (format "%s:%d" file (line-number-at-pos)))))

(defun jv/kill-file ()
  (interactive)
  (jv/kill (jv/buffer-path)))

(defun jv/kill-project-file ()
  (interactive)
  (jv/kill (jv/project-buffer-path)))

(defun jv/kill-line ()
  (interactive)
  (jv/kill-location (jv/buffer-path)))

(defun jv/kill-project-line ()
  (interactive)
  (jv/kill-location (jv/project-buffer-path)))

(define-prefix-command 'spc-kill-map)
(keymap-set spc-map "k" '("kill" . spc-kill-map))
(keymap-set spc-kill-map "f" '("file (project)" . jv/kill-project-file))
(keymap-set spc-kill-map "F" '("file (absolute)" . jv/kill-file))
(keymap-set spc-kill-map "l" '("line (project)" . jv/kill-project-line))
(keymap-set spc-kill-map "L" '("line (absolute)" . jv/kill-line))

(defun jv/completion-at-point ()
  (interactive)
  (if (bound-and-true-p wingman-mode)
      (wingman-fim)
    (completion-at-point)))

(use-package agent-shell
  :after evil
  :config

  (evil-define-key 'insert agent-shell-mode-map (kbd "RET") #'newline)
  (evil-define-key 'normal agent-shell-mode-map (kbd "RET") #'comint-send-input)

  (add-hook 'diff-mode-hook
            (lambda ()
              (when (string-match-p "\\*agent-shell-diff\\*" (buffer-name))
                (evil-emacs-state))))

  (defun jv/agent-shell-dot-subdir (subdir)
    (let* ((cwd (string-remove-suffix "/" (agent-shell-cwd)))
           (sanitized (replace-regexp-in-string "/" "-" (string-remove-prefix "/" cwd))))
      (expand-file-name subdir (locate-user-emacs-file (concat "agent-shell/" sanitized)))))
  (setopt agent-shell-dot-subdir-function #'jv/agent-shell-dot-subdir)

  (when (string= (system-name) "juris-work-laptop")
    (setopt agent-shell-preferred-agent-config
            (agent-shell-anthropic-make-claude-code-config))))

(use-package all-the-icons)

(use-package atomic-chrome
  :vc (:url "https://github.com/KarimAziev/atomic-chrome" :rev newest)
  :commands (atomic-chrome-start-server)
  :config (atomic-chrome-start-server))

(use-package avy
  :general
  (:states '(normal motion)
   "gc" '("go to char" . evil-avy-goto-char-timer)
   "gw" '("go to word" . evil-avy-goto-word-1)
   "gl" '("go to line" . evil-avy-goto-line)))

(use-package blamer
  :config
  (global-blamer-mode 1))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  )

(use-package clipetty
  :hook (after-init . global-clipetty-mode))

(use-package cmake-ts-mode
  :custom
  (cmake-ts-mode-indent-offset 4))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                    ;; Alternative: consult-find
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom
  (completion-in-region-function 'consult-completion-in-region)

  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  ;; (setq xref-show-xrefs-function #'consult-xref
  ;;       xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-narrow-key "<")
  )

(use-package corfu
  :init
  (global-corfu-mode))

(use-package c-ts-mode
  :custom
  (c-ts-mode-indent-offset 4))

(use-package dash
  :config (global-dash-fontify-mode 1))

(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode 1)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package diff-mode
  :defer t
  :config
  (set-face-attribute 'diff-refine-changed nil :extend t)
  (set-face-attribute 'diff-refine-removed nil :extend t)
  (set-face-attribute 'diff-refine-added   nil :extend t))

(use-package dimmer
  :config
  (dimmer-mode))

(use-package dirvish
  :general
  ("C-x d" #'dirvish)
  ("M-s D" #'dirvish-fd)
  (:states 'normal
   :keymaps 'dirvish-mode-map
   :packages '(dired dirvish)
   "q" #'dirvish-quit)
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-attributes
   '(subtree-state all-the-icons collapse git-msg vc-state file-time file-size))
  :config
  (require 'dirvish-fd))

(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width-start 't))

(use-package doom-modeline
  :vc (:url "https://github.com/seagle0128/doom-modeline" :rev newest)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-column-zero-based nil)
  (doom-modeline-percent-position '(-3 "%o"))
  (doom-modeline-enable-word-count t)
  (doom-modeline-continuous-word-count-modes '(markdown-mode org-mode))
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-total-line-number t)
  (doom-modeline-vcs-max-length 32)
  :init (doom-modeline-mode 1)
  :config
  (add-to-list 'nerd-icons-mode-icon-alist
               '(ghostel-mode nerd-icons-faicon "nf-fa-terminal" :face nerd-icons-lblue)))

(use-package dtrt-indent
  :vc (:url "https://github.com/jscheid/dtrt-indent" :rev newest)
  :config
  (dtrt-indent-global-mode 1))

(use-package dumb-jump
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (xref-show-definitions-function #'consult-xref)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))

(use-package ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package eglot
  :config
  :init
  (add-hook 'python-mode-hook 'eglot-ensure)
  :general
  (:keymaps 'spc-lsp-map
   "d" '("definition" . xref-find-definitions)
   "r" '("references" . xref-find-references)
   "h" '("declaration" . eglot-find-declaration)
   "i" '("implementation" . eglot-find-implementation)
   "t" '("typedef" . eglot-find-typeDefinition)
   "f" '("format" . eglot-format)
   "m" '("rename" . eglot-rename)
   "a" '("actions" . eglot-code-actions))
  :config
  ;; (add-hook 'after-save-hook 'eglot-format)
  (add-to-list 'eglot-server-programs
               '(python-base-mode . ("uvx" "--with" "ruff" "--with" "ty" "--from" "rassumfrassum" "rass" "python")))
  (add-to-list 'eglot-server-programs
               '(ruby-base-mode . '("solargraph" "stdio"))))

(use-package emacs
  :bind
  (("C-<mouse-1>" . xref-find-definitions-at-mouse)
   ("C-<mouse-3>" . evil-jump-backward)
   ("C-s" . save-buffer)
   ("C-<return>" . jv/completion-at-point))
  :custom
  ;; Use TAB for autocomplete
  (tab-always-indent t)
  ;; Disable Ispell completion function.
  (text-mode-ispell-word-completion nil)
  ;; Filtering M-x commands.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Nil means single space.
  (sentence-end-double-space nil)
  ;; No backups on save.
  (make-backup-files nil)
  ;; No auto save.
  (auto-save-default nil)
  ;; No locks (symlinks starting with ".#").
  (create-lockfiles nil)
  ;; Permit minibuffer commands while in minibuffer.
  (enable-recursive-minibuffers t)
  ;; Longer lines (default: 70).
  (fill-column 80)
  ;; Use spaces for indentation
  (indent-tabs-mode nil)
  ;; Refuse cursor in minibuffer prompt.
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Use English month and day-of-week names in org-mode.
  (system-time-locale "C")
  :hook
  ((minibuffer-setup-hook . cursor-intangible-mode)
   (emacs-lisp-mode-hook . (lambda () (setq-local lisp-indent-function
                                                  #'emacs-lisp-indent-function))))
  :init
  (global-completion-preview-mode 1))

(use-package emacs-lisp-indent-function
  :load-path "lisp/"
  :autoload emacs-lisp-indent-function)

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-respect-visual-line-mode 't)
  :config
  (add-hook 'pdf-view-mode-hook
            (lambda ()
              (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))
  (evil-define-key '(emacs normal visual) 'global (kbd "SPC") 'spc-map)
  (evil-define-key '(insert visual motion) 'global (kbd "C-SPC") 'spc-map)
  (evil-set-undo-system 'undo-redo)
  (evil-mode 1))

(use-package evil-args
  :bind (:map evil-inner-text-objects-map
         ("a" . 'evil-inner-arg)
         :map evil-outer-text-objects-map
         ("a" . 'evil-outer-arg)
         :map evil-normal-state-map
         ("L" . 'evil-forward-arg)
         ("H" . 'evil-backward-arg)
         :map evil-motion-state-map
         ("L" . 'evil-forward-arg)
         ("H" . 'evil-backward-arg)))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-ghostel
  :vc (:url "https://github.com/dakra/ghostel"
       :lisp-dir "extensions/evil-ghostel"
       :rev :newest)
  :after (ghostel evil)
  :hook (ghostel-mode . evil-ghostel-mode))

(use-package find-file
  :general (:states 'normal
            "C-w e" #'ff-find-other-file
            "C-w E" #'ff-find-other-file-other-window))

(use-package ghostel
  :vc (:url "https://github.com/dakra/ghostel"
       :lisp-dir "lisp"
       :rev :newest)
  :general
  (:keymaps 'spc-map
   "t" '("terminal" . jv/terminal)
   "p" '("pi" . jv/pi))
  :config
  (defun jv/terminal ()
    (interactive)
    (let ((ghostel-buffer-name
           (format "*ghostel: %s*"
                   (abbreviate-file-name default-directory))))
      (call-interactively #'ghostel)))

  (defun jv/run-in-new-terminal (program &optional args)
    (let ((buf (generate-new-buffer (format "*%s*" program))))
      (pop-to-buffer buf)
      (ghostel-exec buf program args)))

  (defun jv/pi ()
    (interactive)
    (jv/run-in-new-terminal "pi")))

(use-package git-auto-commit-mode)

(use-package groovy-ts-mode
  :load-path "lisp/")

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :config
  (when (version< emacs-version "30")
    (setq indent-bars-prefer-character t))
  (require 'indent-bars-ts))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode))

(use-package lua-mode)

(use-package magit
  :defer t
  :commands (magit-add-section-hook)
  :bind
  (:map evil-normal-state-map
   ("g m m" . magit)
   ("g m b" . magit-blame))
  :config
  (require 'all-the-icons)
  (setopt magit-format-file-function #'magit-format-file-all-the-icons)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init (marginalia-mode 1))

(use-package markdown-mode)

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :general
  (:keymaps 'spc-org-map
   "l" 'org-store-link
   "a" 'org-agenda
   "c" 'org-capture)
  :custom
  (org-timestamp-formats ("%Y-%m-%d" . "%Y-%m-%d %H:%M"))
  (org-directory "~/Notes")
  (org-capture-templates
   '(("j" "Journal" entry (file+olp+datetree "journal.org")
      "* %U\n%i%?")))
  )

;; (use-package org-roam
;;   :demand t
;;   :custom
;;   (org-roam-directory "~/Documents/Notes")
;;   (org-roam-dailies-directory "Daily")
;;   (org-roam-completion-everywhere t)
;;   (org-roam-capture-templates
;;    '(("d" "default" plain
;;       "%?"
;;       :if-new (file+head "%<%Y-%m-%d %H:%M:%S> ${title}.org" "#+title: ${title}\n")
;;       :unnarrowed t)))
;;   (org-roam-dailies-capture-templates
;;    '(("d" "default" entry
;;       "* %<%H:%M:%S> %?"
;;       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n i" . org-roam-node-insert)
;;          :map org-mode-map
;;          ("C-M-i" . completion-at-point)
;;          :map org-roam-dailies-map
;;          ("Y" . org-roam-dailies-capture-yesterday)
;;          ("T" . org-roam-dailies-capture-tomorrow))
;;   :bind-keymap
;;   ("C-c n d" . org-roam-dailies-map)
;;   :config
;;   (require 'org-roam-dailies)
;;   (org-roam-db-autosync-mode))

(use-package pdf-tools
  :config
  (pdf-loader-install))

(use-package poly-erb
  :config
  (define-hostmode poly-json-hostmode :mode 'json-ts-mode)
  (defvar poly-json-root-polymode
    (pm-polymode :name "json-root" :hostmode 'poly-json-hostmode)
    "JSON root configuration.")
  (define-polymode poly-json+erb-mode poly-json-root-polymode
    :innermodes '(poly-erb-innermode)))

(use-package project-x
  :vc (:url "https://github.com/vmargb/project-x" :rev newest)
  :after project
  :config
  (add-hook 'project-find-functions 'project-x-try-local 90))

(use-package protobuf-mode
  :mode "\\.proto\\'")

(use-package recentf
  :custom
  (recentf-max-saved-items 1024)
  :config
  (add-to-list 'recentf-exclude "^/\\(?:su\\|sudo\\)?:")
  (run-at-time nil (* 5 60) 'recentf-save-list)
  (recentf-mode 1))

(use-package review-captions
  :load-path "lisp/"
  :bind (("C-c c" . review-captions)))

(use-package rust-mode)

(use-package savehist
  :config
  (savehist-mode 1))

(use-package server
  :config (unless (server-running-p) (server-mode)))

(use-package shell-maker
  :vc (:url "https://github.com/xenodium/shell-maker" :rev newest))

(use-package solaire-mode
  :config (solaire-global-mode 1))

(use-package tramp
  :custom
  (tramp-use-ssh-controlmaster-options nil))

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.cppm\\'" . c++-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode))
  :config
  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (groovy "https://github.com/murtaza64/tree-sitter-groovy")
          (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (ruby-mode . ruby-ts-mode)))
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (unless (treesit-language-available-p lang)
        (treesit-install-language-grammar lang)))))

(use-package vertico
  :init (vertico-mode 1))

(use-package visual-fill-column
  :custom
  (visual-fill-column-center-text t)
  :init
  (defun soft-wrap-mode ()
    (interactive)
    (visual-line-mode 'toggle)
    (visual-fill-column-mode 'toggle))
  :hook (text-mode . soft-wrap-mode))

(use-package welcome
  :load-path "lisp/"
  :config
  (setq initial-buffer-choice #'welcome-buffer))

(use-package which-key
  :custom
  (which-key-sort-uppercase-first nil)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-idle-delay 0.5)
  :config
  (which-key-mode))

(use-package wingman
  :vc (:url "https://github.com/mjrusso/wingman/" :rev newest)
  :general
  (:keymaps 'spc-map
   "w" '("wingman" . global-wingman-mode))
  (:keymaps 'wingman-mode-completion-transient-map
   "f" 'wingman-accept-full
   "l" 'wingman-accept-line
   "w" 'wingman-accept-word)
  :custom
  (wingman-prefix-key nil)
  (wingman-auto-fim nil)
  (wingman-llama-endpoint "http://127.0.0.1:3000/upstream/qwen3.6-27b/infill"))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

(use-package yaml-ts-mode
  :load-path "lisp/")

(add-hook 'after-init-hook
          (lambda ()
            (message "Loading Emacs...done (%.3fs)"
                     (float-time (time-subtract (current-time)
                                                before-init-time))))
          t)
