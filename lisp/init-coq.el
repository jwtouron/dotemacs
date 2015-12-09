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

(provide 'init-coq)
