;; -*- lexical-binding: t; -*-

;;; bootstrapping

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-always-defer t)

;;; packages

(use-package my-defuns
  :straight nil
  :load-path "lisp"
  :commands (my-set-font nd narrow-or-widen-dwim))

(use-package my-defcustoms
  :straight nil
  :load-path "lisp"
  :demand t)

(use-package better-defaults
  :demand t
  :custom
  (indicate-empty-lines t)
  (inhibit-splash-screen t)
  (initial-major-mode 'fundamental-mode)
  (show-trailing-whitespace t)
  (tab-always-indent 'complete)
  :init (defun helm-mode nil) ; Prevents ido-mode from being enabled
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (load custom-file t)
  (global-hl-line-mode)
  (global-auto-revert-mode)
  (column-number-mode t)
  ;; performance
  (when (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq read-process-output-max (* 10 1000 1000)))

(use-package ace-window
  :bind ("C-x o" . 'ace-window))

(use-package avy
  :bind ("C-;" . 'avy-goto-char-timer))

(use-package company
  :custom (company-idle-delay nil)
  :hook (prog-mode . company-mode)
  :bind
  ((:map company-mode-map
         ("TAB" . 'company-indent-or-complete-common))
   (:map company-active-map
         ("C-n" . 'company-select-next)
         ("C-p" . 'company-select-previous))))

(use-package compilation-mode
  :straight nil
  :custom ((compilation-ask-about-save nil))
  :init
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(use-package consult
  :commands (consult--grep)
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complet-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("C-c r l" . consult-register-load)
         ("C-c r s" . consult-register-store)
         ("C-c r r" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-project-imenu)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s L" . consult-locate)
         ("M-s g" . my-consult-grep)
         ("C-c g" . my-consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line))
  :init
  (defun my-consult-grep (&optional dir initial)
    (interactive "P")
    (cond
     ((executable-find "ugrep")
      (let ((command "ugrep -RInk -j --tabs=1 --ignore-files --null --line-buffered -e ARG OPTS"))
        (consult--grep "ugrep" command dir initial)))
     ((executable-find "rg")
      (consult-ripgrep dir initial))
     (t
      (consult-grep dir initial))))
  (setq register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-preview-key (kbd "`")
        consult-narrow-key "<"
        consult-project-root-function #'projectile-project-root))

(use-package crux)

(use-package diminish)

(use-package dired+
  :straight nil
  :defer 1
  :config (diredp-toggle-find-file-reuse-dir 1))

(use-package dot-mode
  :defer 1
  :diminish 'dot-mode
  :init (global-dot-mode t))

(use-package double-saber
  :if nil
  :demand t
  :hook (grep-mode . (lambda ()
                       (message "ivy-occur-mode-hook")
                       (double-saber-mode)
                       (setq-local double-saber-start-line 5))))

(use-package dumb-jump
  :bind (("C-M-j" . dumb-jump-hydra/body))
  :hook (prog-mode . dumb-jump-mode)
  ;; :custom (dumb-jump-selector 'ivy)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")
    ("q" nil "Quit")))

(use-package easy-kill
  :if nil
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package embark
  :bind ("C-S-a" . embark-act))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package expand-region
  :bind ("C-=" . #'my-expand-region)
  :init
  (defun my-expand-region ()
    (interactive)
    (er/expand-region 1)
    (hydra-expand-region/body))
  :config
  (defhydra hydra-expand-region ()
    "expand-region"
    ("=" er/expand-region "er/expand-region")
    ("C-=" er/expand-region "er/expand-region")
    ("-" er/contract-region "er/contract-region")
    ("C--" er/contract-region "er/contract-region")))

(use-package flycheck)

(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode)
  :custom (flycheck-pos-tip-timeout 0))

(use-package gcmh
  :defer 1
  :diminish 'gcmh-mode
  :init (gcmh-mode 1))

(use-package hippie-expand
  :straight nil
  :custom
  (hippie-expand-try-functions-list '(try-expand-dabbrev-visible
                                      try-expand-dabbrev
                                      try-expand-line
                                      try-expand-dabbrev-all-buffers
                                      try-complete-file-name-partially
                                      try-complete-file-name)))

(use-package hydra
  :bind (("M-g M-n" . my-next-error)
         ("M-g M-p" . my-previous-error)
         ;; ("C-n" . (lambda () (interactive) (next-line) (hydra-ctrl-move/body)))
         ;; ("C-p" . (lambda () (interactive) (previous-line) (hydra-ctrl-move/body)))
         ;; ("C-a" . (lambda () (interactive) (move-beginning-of-line 1) (hydra-ctrl-move/body)))
         ;; ("C-e" . (lambda () (interactive) (move-end-of-line 1) (hydra-ctrl-move/body)))
         ;; ("C-f" . (lambda () (interactive) (forward-char) (hydra-ctrl-move/body)))
         ;; ("C-b" . (lambda () (interactive) (backward-char) (hydra-ctrl-move/body)))
         ;; ("C-o" . (lambda () (interactive) (open-line) (hydra-ctrl-move/body)))
         ;; ("C-k" . (lambda () (interactive) (kill-line) (hydra-ctrl-move/body)))
         ;; ("M-f" . (lambda () (interactive) (forward-word) (hydra-meta-move/body)))
         ;; ("M-b" . (lambda () (interactive) (backward-word) (hydra-meta-move/body)))
         ;; ("C-M-f" . (lambda () (interactive) (forward-sexp) (hydra-sexp-move/body)))
         ;; ("C-M-b" . (lambda () (interactive) (backward-sexp) (hydra-sexp-move/body)))
         ;; ("C-M-u" . (lambda () (interactive) (backward-up-list) (hydra-sexp-move/body)))
         ;; ("C-M-d" . (lambda () (interactive) (down-list) (hydra-sexp-move/body)))
         ;; ("C-M-k" . (lambda () (interactive) (kill-sexp) (hydra-sexp-move/body)))
         )
  :init
  (defun my-next-error ()
    (interactive)
    (next-error)
    (hydra-navigate-errors/body))
  (defun my-previous-error ()
    (interactive)
    (previous-error)
    (hydra-navigate-errors/body))
  :config
  (defhydra hydra-navigate-errors ()
    "navigate-errors"
    ("M-n" next-error "next-error")
    ("M-p" previous-error "previous-error"))
  (defhydra hydra-ctrl-move (:hint nil)
    ""
    ("C-n" next-line)
    ("n" next-line)
    ("C-p" previous-line)
    ("p" previous-line)
    ("C-a" (lambda () (interactive) (move-beginning-of-line 1)))
    ("a" move-beginning-of-line)
    ("C-e" (lambda () (interactive) (move-end-of-line 1)))
    ("e" move-end-of-line)
    ("C-f" forward-char)
    ("f" forward-char)
    ("C-b" backward-char)
    ("b" backward-char)
    ("C-o" open-line)
    ("o" open-line)
    ("C-k" kill-line)
    ("k" kill-line)
    ("q" nil))
  (defhydra hydra-meta-move (:hint nil)
    ""
    ("M-f" forward-word)
    ("f" forward-word)
    ("M-b" backward-word)
    ("b" backward-word)
    ("q" nil))
  (defhydra hydra-sexp-move (:hint nil)
    ""
    ("C-M-f" forward-sexp)
    ("f" forward-sexp)
    ("C-M-b" backward-sexp)
    ("b" backward-sexp)
    ("C-M-u" backward-up-list)
    ("u" backward-up-list)
    ("C-M-d" down-list)
    ("d" down-list)
    ("C-M-k" kill-sexp)
    ("k" kill-sexp)
    ("q" nil)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom ((lsp-keymap-prefix "C-c l")
           (lsp-enable-symbol-highlighting nil)
           (lsp-enable-snippet nil)
           (lsp-headerline-breadcrumb-enable nil))
  :hook (lsp-mode . lsp-enable-which-key-integration))

(use-package magit)

(use-package paredit)

(use-package projectile
  :commands (projectile-project-root)
  :bind
  ((:map projectile-mode-map
         ("C-c p" . 'projectile-command-map)))
  :init (projectile-mode +1)
  :config
  (setq projectile-indexing-method 'alien
        ;; projectile-completion-system 'ivy
        ))

(use-package recentf
  :defer 1
  :init (recentf-mode))

(use-package selectrum
  :init (selectrum-mode +1))

(use-package selectrum-prescient
  :defer 1
  :init
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package smartscan
  :defer 1
  :init (global-smartscan-mode 1))

(use-package super-save
  :defer 1
  :diminish 'super-save-mode
  :init (super-save-mode +1)
  :config (setq auto-save-default nil))

(use-package undo-tree
  :defer 1
  :diminish 'undo-tree-mode
  :init (global-undo-tree-mode))

(use-package visual-regexp
  :bind (("M-%" . vr/query-replace)
         ("C-M-%" . vr/replace)))

(use-package wgrep)

(use-package which-key
  :defer 1
  :diminish 'which-key-mode
  :init (which-key-mode))

;;; languages

;; C/C++

(add-hook 'c-mode-common-hook (lambda () (electric-pair-local-mode 1)))
(c-add-style "my-c-style"
             '("stroustrup"
               (c-offsets-alist
                (case-label . +))))
(setq-default c-default-style
              '((java-mode . "java")
                (awk-mode . "awk")
                (other . "my-c-style")))

;; emacs lisp

(use-package elisp-mode
  :straight nil
  :hook ((emacs-lisp-mode . paredit-mode)))

;; haskell

(use-package haskell-mode
  :hook ((haskell-mode . electric-pair-local-mode)
;;         (haskell-mode . lsp-deferred)
         (haskell-mode . flycheck-mode)

         (haskell-mode . interactive-haskell-mode)
         (haskell-mode . my-haskell-minor-mode))
  :custom ((haskell-process-use-presentation-mode t))
  :init
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-c m e") 'my-haskell-eval)
    (define-key keymap (kbd "C-c m h") 'haskell-hoogle)
    (define-key keymap (kbd "C-c m r") 'haskell-process-restart)
    (define-key keymap (kbd "C-c m t") 'my-haskell-eval-type)
    (define-minor-mode my-haskell-minor-mode
      "Sets up my settings for Haskell"
      :keymap keymap
      (set (make-local-variable 'company-backends) '(company-dabbrev-code))))
  :config
  (defun my-haskell-eval-type ()
    (interactive)
    (let ((line (if (region-active-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (symbol-at-point)) ))
      (haskell-process-show-repl-response (format ":t %s" line))))
  (defun my-haskell-eval ()
    (interactive)
    (let ((line (if (region-active-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (string-trim-left (buffer-substring-no-properties (line-beginning-position)
                                                                    (line-end-position))
                                    " *-- *"))))
      (haskell-process-show-repl-response line))))

(use-package lsp-haskell)

;; nim

(use-package nim-mode)

(use-package nxml-mode
  :straight nil
  :hook ((nxml-mode . electric-pair-local-mode)))

;; python

(use-package python
  :straight nil
  :custom (python-shell-interpreter "python3")
  :hook ((python-mode . electric-pair-local-mode)))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; yaml

(use-package yaml-mode)

;;; theme

(use-package nano-theme
  :if nil
  :commands (nano-dark)
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme")
  :init (nano-dark))

(use-package emacs-material-theme
  :if nil
  :straight (emacs-material-theme :type git :host github :repo "cpaulik/emacs-material-theme")
  :init (load-theme 'material t))

(use-package modus-vivendi-theme
  :if nil
  :init (load-theme 'modus-vivendi t))

(use-package vscode-dark-plus-theme
  :if nil
  :init (load-theme 'vscode-dark-plus t))

(use-package warm-night-theme
  :init (load-theme 'warm-night t))

(custom-set-faces
 '(show-paren-match ((t (:underline t :foreground nil :background nil)))))
