;;; tools.el -*- lexical-binding: t -*-

;; Code folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; Optional tree sidebar
(use-package treemacs
  :defer t
  :custom
  (treemacs-width 30)
  (treemacs-is-never-other-window t))

;; symbol highlight
(use-package symbol-overlay
  :defer t
  :hook ((prog-mode . symbol-overlay-mode))
  :config
  ;; 高亮光标下 symbol
  (setq symbol-overlay-idle-time 0.3))
