;;; misc.el --- core settings -*- lexical-binding: t -*-

;; project.el
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


;; which-key
(use-package which-key
  :defer 1
  :init
  (setq which-key-idle-delay 0.4
        which-key-separator " → "
        which-key-min-display-lines 2)
  :config
  (which-key-mode 1))

;; imenu
(setq imenu-auto-rescan t
      imenu-use-popup-menu nil)


;; code folding
;; 只在 prog-mode 启用折叠
(add-hook 'prog-mode-hook #'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(add-hook 'markdown-mode-hook #'outline-minor-mode)
(add-hook 'org-mode-hook #'outline-minor-mode)

;; 折叠时不隐藏当前行
(setq outline-blank-line t)

;; 搜索时自动展开折叠
(setq hs-isearch-open t)


(provide 'misc)
