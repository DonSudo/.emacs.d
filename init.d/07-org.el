;;; org.el -*- lexical-binding: t -*-

(use-package org
  :defer t
  :config
  (setq org-startup-indented t
        org-hide-leading-stars t
        org-ellipsis " ▾"))

;; modern
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))
