(require 'cl) ;; Required by many packages

;;;; My Functions

(defun my-open-block-c-mode ()
  (newline)
  (newline)
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

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

;;;; Packages

(req-package init-evil
  :init (setq evil-want-C-u-scroll t))

(req-package better-defaults)
(req-package init-misc :require better-defaults)

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
    (require 'smartparens-config)
    (smartparens-global-mode t)))

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

(req-package flycheck
  :config (add-hook 'prog-mode-hook 'flycheck-mode))

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

(req-package dired+)
(req-package impatient-mode)
(req-package web-mode)

(req-package init-langs)

(req-package-finish)

;;;; Theme

(require 'init-theme)
(my-load-random-theme)
