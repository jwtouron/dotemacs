;;; C/C++
(setq-default c-default-style "stroustrup")
(req-package company-c-headers
  :config (add-to-list 'company-backends 'company-c-headers))

;;;; Coq
(req-package init-coq)
(req-package company-coq
  :require init-coq)

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
                (haskell-indentation-mode 1)
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

(provide 'init-langs)
