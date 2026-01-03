;;; core-pkg.el --- core settings -*- lexical-binding: t -*-

;; ------------------------------
;; org-mode.el
;; ------------------------------
(use-package org
  :init
  (setq org-startup-indented nil
        org-pretty-entities t
        org-hide-leading-stars t
        org-ellipsis " ▾"))


;; ------------------------------
;; project.el
;; ------------------------------
(use-package project
  :defer t
  :init
  ;; Windows / 大仓库下性能优化
  (setq project-vc-extra-root-markers nil)

  :config
  ;; project.el 默认使用 vc
  (setq project-switch-commands
        '((project-find-file "Find file")
          (consult-project-buffer "Buffer")
          (project-dired "Dired")
          (consult-ripgrep "Ripgrep"))))


;; ------------------------------
;; which-key
;; ------------------------------
(use-package which-key
  :defer 1
  :init
  (setq which-key-idle-delay 0.4
        which-key-separator " → "
        which-key-min-display-lines 2)
  :config
  (which-key-mode 1))


(provide 'core-pkg)
