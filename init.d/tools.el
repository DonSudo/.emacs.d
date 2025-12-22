;;; tools.el -*- lexical-binding: t -*-

;; symbol highlight
(use-package symbol-overlay
  :defer t
  :hook ((prog-mode . symbol-overlay-mode))
  :config
  ;; 高亮光标下 symbol
  (setq symbol-overlay-idle-time 0.3))

;; yank / delete 高亮（支持 yy / Y）
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.15))

;;  move line like VS Code
(use-package move-dup
  :bind
  ([S-M-down] . move-dup-duplicate-down)
  ([S-M-up] . move-dup-duplicate-up)
  ([M-up] . move-dup-move-lines-up)
  ([M-down] . move-dup-move-lines-down))

(provide 'tools)
