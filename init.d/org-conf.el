;;; org-conf.el -*- lexical-binding: t -*-

(use-package org
  :init
  (setq org-startup-indented nil
        org-hide-leading-stars t
        org-ellipsis " ▾"))

;; modern
(use-package org-modern
  :after org
  :hook
  (org-mode . org-modern-mode))

;; 搭配 evil
(defvar-local my/org-modern--was-enabled nil)

(defun my/org-modern-disable ()
  (when (and (derived-mode-p 'org-mode)
             (bound-and-true-p org-modern-mode))
    (setq my/org-modern--was-enabled t)
    (org-modern-mode -1)))

(defun my/org-modern-enable ()
  (when (and (derived-mode-p 'org-mode)
             my/org-modern--was-enabled)
    (org-modern-mode 1)
    (setq my/org-modern--was-enabled nil)))

;; 进入 insert mode → 关闭渲染
(add-hook 'evil-insert-state-entry-hook #'my/org-modern-disable)

;; 离开 insert mode（回到 normal / visual / motion）→ 恢复渲染
(add-hook 'evil-insert-state-exit-hook  #'my/org-modern-enable)


(provide 'org-conf)
