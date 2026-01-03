;;; vim-core.el -*- lexical-binding: t -*-

(use-package evil
  :init
  (setq evil-want-Y-yank-to-eol t
        evil-want-C-i-jump nil
        evil-disable-insert-state-bindings t
        evil-want-keybinding nil
        evil-want-integration t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(setq evil-ex-search-count t)

;; ensure minibuffer keep emacs mode
(with-eval-after-load 'evil
  (evil-set-initial-state 'minibuffer-mode 'emacs))


(provide 'vim-core)
