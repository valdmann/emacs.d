;; -*- lexical-binding: t -*-

(defun jv/completion-at-point ()
  (interactive)
  (if (bound-and-true-p wingman-mode)
      (wingman-fim)
    (completion-at-point)))

(use-package all-the-icons)

(use-package avy
  :general
  (:states '(normal motion)
   "gc" '("go to char" . evil-avy-goto-char-timer)
   "gw" '("go to word" . evil-avy-goto-word-1)
   "gl" '("go to line" . evil-avy-goto-line)))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  )

(use-package c-ts-mode
  :custom
  (c-ts-mode-indent-offset 4))

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

(use-package dirvish
  :general ("C-x d" #'dirvish)
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
  :init (doom-modeline-mode 1))

(use-package indent-bars
  :hook (prog-mode . indent-bars-mode)
  :config
  (when (version< emacs-version "30")
    (setq indent-bars-prefer-character t))
  (require 'indent-bars-ts))

(use-package easy-kill
  :bind
  ([remap kill-ring-save] . easy-kill)
  ([remap mark-sexp] . easy-mark))


(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(ruby-base-mode "solargraph" "stdio")))

(use-package emacs
  :bind
  (("C-<mouse-1>" . xref-find-definitions-at-mouse)
   ("C-<mouse-3>" . evil-jump-backward)
   ("C-<return>" . jv/completion-at-point))
  :custom
  ;; Use TAB for autocomplete
  (tab-always-indent t)
  ;; Disable Ispell completion function.
  ;; (text-mode-ispell-word-completion nil)
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

(use-package find-file
  :general (:states 'normal
            "C-w e" #'ff-find-other-file
            "C-w E" #'ff-find-other-file-other-window))

(use-package git-auto-commit-mode)

(use-package gptel
  :custom
  (gptel-model 'qwen3-vl-8b-instruct)
  (gptel-track-media 't)
  :config
  (setq gptel-backend
        (gptel-make-openai "kurk"
          :host "kurk:3000"
          :stream t
          :protocol "http"
          :models '(
                    ;; curl -s kurk:3000/v1/models | jq -r '.data[].id'
                    (gemma3-12b-it
                     :capabilities (media)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    gemma3-12b-pt
                    (gemma3-12b-starshine
                     :capabilities (media)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    (gemma3-27b-it
                     :capabilities (media)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    (gemma3-4b-it
                     :capabilities (media)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    (gpt-oss-20b
                     :capabilities (tool-use))
                    (magistral-small-1.2
                     :capabilities (tool-use))
                    mistral-nemo-instruct-2407
                    (mistral-small-3.2
                     :capabilities (tool-use))
                    olmo2-32b-instruct
                    (qwen3-14b
                     :capabilities (tool-use))
                    (qwen3-30b-a3b-thinking
                     :capabilities (tool-use))
                    (qwen3-32b
                     :capabilities (tool-use))
                    (qwen3-4b-instruct
                     :capabilities (tool-use))
                    (qwen3-vl-30b-a3b-instruct
                     :capabilities (media tool-use)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    (qwen3-vl-32b-instruct.iq4_xs
                     :capabilities (media tool-use)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    (qwen3-vl-32b-instruct.q3_k_xl
                     :capabilities (media tool-use)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    (qwen3-vl-32b-thinking
                     :capabilities (media tool-use)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    (qwen3-vl-4b-instruct
                     :capabilities (media tool-use)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    (qwen3-vl-8b-instruct
                     :capabilities (media tool-use)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    (qwen3-vl-8b-thinking
                     :capabilities (media tool-use)
                     :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp"))
                    (seed-oss-36b-instruct
                     :capabilities (tool-use))
                    ))))

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

(use-package org-roam
  :demand t
  :custom
  (org-roam-directory "~/Documents/Notes")
  (org-roam-dailies-directory "Daily")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y-%m-%d %H:%M:%S> ${title}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  (org-roam-dailies-capture-templates
   '(("d" "default" entry
      "* %<%H:%M:%S> %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

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

(use-package solaire-mode
  :config (solaire-global-mode 1))

(use-package tramp
  :custom
  (tramp-use-ssh-controlmaster-options nil))

(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :config
  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
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

(use-package wingman
  :vc (:url "https://github.com/mjrusso/wingman/" :rev newest)
  :bind
  ("C-c w" . global-wingman-mode)
  (:map wingman-mode-completion-transient-map
   ("C-f" . wingman-accept-full)
   ("C-e" . wingman-accept-line)
   ("C-w" . wingman-accept-word))
  :custom
  (wingman-prefix-key nil)
  :config
  (setq wingman-auto-fim nil)
  (setq wingman-llama-endpoint "http://kurk:3000/upstream/qwen3-coder-30b-a3b/infill"))

(use-package which-key
  :custom
  (which-key-sort-uppercase-first nil)
  (which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-mode))

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
