;;; edit-format -*- lexical-binding: t -*-

(use-package apheleia
  :hook (prog-mode  .  apheleia-mode)
  :config
  ;; 保存时自动格式化
  (setq apheleia-global-mode -1))


(with-eval-after-load 'apheleia
  ;; C / C++
  (setf (alist-get 'c-mode apheleia-mode-alist) 'clang-format
        (alist-get 'c++-mode apheleia-mode-alist) 'clang-format)

  ;; Python
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff)

  ;; JavaScript / TypeScript / JSON / CSS
  (setf (alist-get 'js-mode apheleia-mode-alist) 'prettier
        (alist-get 'typescript-mode apheleia-mode-alist) 'prettier
        (alist-get 'tsx-mode apheleia-mode-alist) 'prettier
        (alist-get 'json-mode apheleia-mode-alist) 'prettier
        (alist-get 'css-mode apheleia-mode-alist) 'prettier)

  ;; Rust
  (setf (alist-get 'rust-mode apheleia-mode-alist) 'rustfmt)

  ;; Go
  (setf (alist-get 'go-mode apheleia-mode-alist) 'gofmt)

  ;; Lua
  (setf (alist-get 'lua-mode apheleia-mode-alist) 'stylua))


(add-hook 'prog-mode-hook #'apheleia-mode)

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "g=") #'apheleia-format-buffer))


(provide 'edit-format)
