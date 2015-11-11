;; TODO
;; move lang specific down
;; smartparens open brace
;; defadvice multiple cursors
;; lighten theme
;; recentf in C-x b

(require 'cl) ;; Required by many packages

;;;; packages

;;; package

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)

;;; req-package

(unless (package-installed-p 'req-package)
  (package-refresh-contents)
  (package-install 'req-package))
(require 'req-package)

;;; misc

(add-to-list 'load-path "~/.emacs.d/lisp/")
;;;; My Functions

(defun my-open-block-c-mode ()
  (newline)
  (newline)
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

;;;; Packages

;;; evil

(req-package evil-leader
  :require evil
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "SPC" 'evil-ex-nohighlight
      "xf" 'ido-find-file
      "xx" 'smex
      "xo" 'other-window
      "x1" 'delete-other-windows
      "x0" 'delete-window
      "xb" 'ido-switch-buffer)))

(req-package evil-nerd-commenter
  :require evil
  :config
  (progn
    (evilnc-default-hotkeys)))

(req-package evil-matchit
  :require evil
  :config (global-evil-matchit-mode 1))

(req-package evil-surround
  :require evil
  :config (global-evil-surround-mode 1))

(req-package evil-visualstar
  :require evil
  :config (global-evil-visualstar-mode t))

(req-package evil-exchange
  :require evil
  :config (evil-exchange-install))

(req-package evil-snipe
  :require evil
  :config
  (progn
    (evil-snipe-mode 1)
    (evil-snipe-override-mode 1)
    (setq evil-snipe-scope 'whole-visible)
    (setq evil-snipe-repeat-scope 'whole-visible)))

(setq evil-want-C-u-scroll t)
(req-package evil
  :require my-init-misc
  :config
  (progn
    (setq evil-search-module 'evil-search)
    (define-key evil-normal-state-map (kbd "/") 'evil-ex-search-forward)
    (define-key evil-normal-state-map (kbd "?") 'evil-ex-search-backward)
    (define-key evil-normal-state-map (kbd "n") 'evil-ex-search-next)
    (define-key evil-normal-state-map (kbd "N") 'evil-ex-search-previous)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
    (evil-mode 1))
  :diminish undo-tree-mode)

(req-package better-defaults)
(req-package my-init-misc :require better-defaults)

(req-package linum-relative
  :require linum
  :init (global-linum-mode 1)
  :config (linum-relative-on))

(req-package ido
  :config
  (progn
    (ido-mode t)
    (ido-everywhere 1)
    (setq ido-enable-flex-matching t)
    (setq ido-case-fold t)
    (setq ido-use-virtual-buffers)))

(req-package ido-ubiquitous
  :require ido
  :config (ido-ubiquitous-mode 1))

(req-package smex
  :config
  (progn
    (smex-initialize)
    (global-set-key (kbd "M-x") 'smex)
    (global-set-key (kbd "M-X") 'smex-major-mode-commands)
    (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

(req-package projectile
  :config
  (progn
    (projectile-global-mode))
  :diminish projectile-mode)

(req-package smartparens
  :config
  (progn
    (smartparens-global-mode 1)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)))

(req-package multiple-cursors
  :config
  (progn
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
    ;; mc/prompt-for-inclusion-in-whitelist ; TODO: advise this
    ))

(req-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package company
  :config (global-company-mode))

;; (let ((backends '(company-anaconda
;;                   company-cabal
;;                   company-ghc
;;                   company-ghci
;;                   company-jedi
;;                   company-web)))
;;   (dolist (backend backends)
;;     (req-package backend
;;       :config (add-to-list 'company-backends backend))))

(req-package flycheck
  :config (add-hook 'prog-mode-hook 'flycheck-mode))

;; (req-package flycheck-clojure
;;   :config (flycheck-clojure-setup))

;; (req-package flycheck-haskell
;;   :config (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))

;; (req-package flycheck-rust
;;   :config (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(req-package ggtags
  :config (add-hook 'prog-mode-hook (lambda () (ggtags-mode 1))))

(req-package lispy
  :config (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1))))

(req-package ace-window
  :config
  (global-set-key (kbd "C-c w") 'ace-window))

(req-package magit
  :config
  (when (eq system-type 'windows-nt)
    (setenv "GIT_ASKPASS" "git-gui--askpass")))

(req-package avy
  :config (global-set-key (kbd "C-'") 'avy-goto-char))

;; (req-package hl-line+
;;   :config (toggle-hl-line-when-idle 1))

(req-package dired+)
(req-package impatient-mode)
(req-package web-mode)

;;;; Languages

;;; C/C++
(setq-default c-default-style "stroustrup")
(req-package company-c-headers
  :config (add-to-list 'company-backends 'company-c-headers))

;;;; Coq
(when (eq system-type 'windows-nt)
  (defvar coq-prog-name "~/.emacs.d/coq/coq-emacs.bat"))
(load-file "~/.emacs.d/coq/ProofGeneral/generic/proof-site.el")
(defadvice proof-electric-terminator (around my-proof-electric-terminator-advice)
  (if proof-electric-terminator-enable
      (let ((p (point)))
        ad-do-it
        (goto-char p)
        (when (and (not (char-equal (char-before) ?.))
                   (char-equal (char-after) ?.))
          (forward-char)))
    ad-do-it))
(add-hook 'coq-mode-hook (lambda () (electric-indent-mode 0)))
(req-package company-coq)

;;; F♯
(req-package fsharp-mode)

;;; Rust
(req-package rust-mode)

;;; Groovy
(req-package groovy-mode)

;;; Lua
(req-package lua-mode)

;;; Racket
(req-package racket-mode)

;;; Clojure
(req-package clojure-mode)
(req-package cider)

;;; Haskell
(req-package haskell-mode
  :config
  (progn
    (add-hook 'haskell-mode-hook
              (lambda ()
                (interactive-haskell-mode)
                (turn-on-haskell-indentation)
                (setq haskell-program-name "stack ghci")
                (turn-on-haskell-doc-mode)
                (define-key haskell-mode-map (kbd "C-c >") 'haskell-move-nested-right)
                (define-key haskell-mode-map (kbd "C-c <") 'haskell-move-nested-left)))))
(req-package company-ghc
  :require company
  :config (add-to-list 'company-backends 'company-ghc))

;;; Javascript
(req-package js2-mode)
(req-package js-comint)

;;; Yaml
(req-package yaml-mode)

;;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key (kbd "RET")
                           (lambda ()
                             (interactive)
                             (if (and (char-equal (char-before) ?\{)
                                      (char-equal (char-after) ?\}))
                                 (my-open-block-c-mode)
                               (newline-and-indent))))))
(req-package javadoc-lookup :require cc-mode)

;;; C♯
(req-package csharp-mode)

(req-package-finish)

;;;; Theme

(setq my-package-themes
      '((zenburn-theme                  . zenburn)
        (solarized-theme                . solarized-dark)
        (heroku-theme                   . heroku)
        (color-theme-sanityinc-tomorrow . sanityinc-tomorrow-eighties)
        (color-theme-sanityinc-tomorrow . sanityinc-tomorrow-night)
        (monokai-theme                  . monokai)
        ;; (moe-theme                      . moe-dark)
        ;; (molokai-theme                  . molokai)
        (tangotango-theme               . tango-dark)
        (tangotango-theme               . tangotango)
        (alect-themes                   . alect-dark)
        (alect-themes                   . alect-dark-alt)
        (alect-themes                   . alect-black)
        (alect-themes                   . alect-black-alt)
        (ample-zen-theme                . ample-zen)
        ;; (noctilux-theme                 . noctilux)
        (ample-theme                    . ample)
        (ample-theme                    . ample-flat)
        (atom-dark-theme                . atom-dark)
        (atom-one-dark-theme            . atom-one-dark)
        ;; (emacs-slime-theme              . emacs-slime)
        (sublime-themes                 . brin)
        ;; (sublime-themes                 . dorsey)
        ;; (sublime-themes                 . fogus)
        ;; (sublime-themes                 . graham)
        ;; (sublime-themes                 . granger)
        ;; (sublime-themes                 . hickey)
        (sublime-themes                 . junio)
        ;; (sublime-themes                 . mccarthy)
        (sublime-themes                 . odersky)
        (sublime-themes                 . ritchie)
        (sublime-themes                 . spolsky)
        (sublime-themes                 . wilson)
        ))

(dolist (theme my-package-themes)
  (unless (package-installed-p (car theme))
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install (car theme))))

(setq my-themes (append my-package-themes))

(defun my-load-random-theme ()
  "Randomly loads an installed theme."
  (interactive)
  (let* ((i (random (length my-themes)))
         (theme (cdr (nth i my-themes))))
    (load-theme theme t)
    (setq my-loaded-theme theme)
    (message (format "Theme loaded: %s" theme))))

(my-load-random-theme)
