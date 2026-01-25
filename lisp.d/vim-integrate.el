;;; vim-integrate.el -*- lexical-binding: t -*-

;; Corfu 只用于 buffer，不进入 minibuffer
(setq corfu-auto t)

(defun my/disable-corfu-in-minibuffer ()
  (setq-local corfu-auto nil)
  (corfu-mode -1))

(add-hook 'minibuffer-setup-hook #'my/disable-corfu-in-minibuffer)

;; yank / delete 高亮（支持 yy / Y）
(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.15))


(provide 'vim-integrate)
