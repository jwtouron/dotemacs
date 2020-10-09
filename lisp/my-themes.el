(use-package emacs-material-theme
  :if nil
  :straight (el-patch :type git :host github :repo "cpaulik/emacs-material-theme")
  :init (load-theme 'material t))

(use-package gruber-darker-theme
  :init (load-theme 'gruber-darker t))

(use-package modus-themes
  :if nil
  :init (modus-themes-load-themes)
  :config (modus-themes-load-vivendi))

(use-package vscode-dark-plus-theme
  :if nil
  :init (load-theme 'vscode-dark-plus t))

(use-package warm-night-theme
  :if nil
  :init (load-theme 'warm-night t))

(custom-set-faces
 '(show-paren-match ((t (:underline t :foreground nil :background nil)))))

(provide 'my-themes)
