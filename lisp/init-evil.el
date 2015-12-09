;;; init-evil

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
  :config (evilnc-default-hotkeys))

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

(req-package evil
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

(provide 'init-evil)
