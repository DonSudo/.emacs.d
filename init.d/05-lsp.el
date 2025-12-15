;;; lsp.el -*- lexical-binding: t -*-

;; tree-sitter
(use-package tree-sitter
  :defer t
  :hook (prog-mode . global-tree-sitter-mode)
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(setq treesit-font-lock-level 4)

(use-package tree-sitter-langs
  :after tree-sitter)

;; eglot
(use-package eglot
  :defer t
  :hook ((c-mode c++-mode python-mode org-mode) . eglot-ensure)
  :config
  (setq eglot-events-buffer-size 0)
  
  ;; 自定义 LSP 路径，可选
  (defvar my/lsp-server-paths nil
    "Alist of major-mode -> custom LSP path. Example: '((python-mode . \"C:/tools/pyright\"))")
  ;; 注册自定义路径
  (dolist (pair my/lsp-server-paths)
    (when (and (car pair) (cdr pair))
      (add-to-list 'eglot-server-programs
                   `(,(car pair) . (,cdr pair "--stdio")))))

  (add-to-list 'completion-at-point-functions #'eglot-completion-at-point)
  (defun my/eglot-symbol-overlay-refresh ()
    (when (bound-and-true-p symbol-overlay-mode)
      (symbol-overlay-refresh)))
  (add-hook 'eglot-managed-mode-hook #'my/eglot-symbol-overlay-refresh))
