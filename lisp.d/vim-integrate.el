;;; vim-integrate.el -*- lexical-binding: t -*-

;; yank / delete 高亮（支持 yy / Y）
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.15))

(provide 'vim-integrate)