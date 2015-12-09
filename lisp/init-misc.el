(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)
(setq-default column-number-mode t)
(setq-default indicate-empty-lines t)
(setq line-move-visual nil)
(ignore-errors (set-frame-font (format "%s-10" "DejaVu Sans Mono")))
(put 'narrow-to-region 'disabled nil)
(setq-default buffer-file-coding-system 'utf-8-unix)
(global-auto-revert-mode t)
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))
(global-hl-line-mode 1)

;;; semantic
(semantic-mode 1)
(global-semantic-idle-completions-mode t)
(global-semantic-show-unmatched-syntax-mode t)

;;; Swap newline keybindings
(global-set-key (kbd "C-j") 'newline)
(global-set-key (kbd "RET") 'newline-and-indent)

;;; Whitespace
(setq-default show-trailing-whitespace t)
(setq-default whitespace-style '(tabs tab-mark))
(global-whitespace-mode 1)
(diminish 'global-whitespace-mode)

;;; Compile
(setq-default compilation-ask-about-save nil)
(setq-default compilation-auto-jump-to-first-error t)
(add-hook 'prog-mode-hook (lambda () (local-set-key (kbd "C-c c") 'compile)))

(provide 'init-misc)
