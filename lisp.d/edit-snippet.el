;;; edit-snippet.el -*- lexical-binding: t -*-

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"   ; 自定义
          "~/.emacs.d/vendor/yasnippet-snippets")))

(provide 'edit-snippet)
