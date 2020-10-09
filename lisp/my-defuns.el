;; -*- lexical-binding: t; -*-

;;;###autoload
(defun my-set-font ()
  "Interactively set the font"
  (interactive)
  (let ((font-name (ivy-read "Font name: " (sort (delete-dups (font-family-list)) 'string-lessp)))
        (font-size (read-string "Font size: ")))
    (customize-save-variable 'my-font `(,font-name . ,(string-to-number font-size)))))

;;;###autoload
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))
(defalias 'nd 'narrow-or-widen-dwim)

;;;###autoload
(defun my-counsel-project-rg ()
  "Search the current project"
  (interactive)
  (let ((project-dir (cdr (project-current))))
    (if project-dir
        (counsel-rg nil project-dir)
      (user-error "Not in a project"))))

(defun align-non-space (beg end)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)\\S-+" 1 1 t))

(provide 'my-defuns)
