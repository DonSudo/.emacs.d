;;; evil.el -*- lexical-binding: t -*-

;; Undo-tree integration
(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-auto-save-history nil))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-Y-yank-to-eol t
        evil-want-C-i-jump nil
        evil-want-keybinding nil
        evil-want-integration t
        evil-undo-system 'undo-tree)
  :config
  (evil-mode 1))


