;;; core-treesit.el -*- lexical-binding: t -*-

(require 'treesit)
(setq treesit-font-lock-level 4)

;; grammar 动态库搜索路径
(setq treesit-extra-load-path
      (list (expand-file-name "tree-sitter" user-emacs-directory)))

(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4")
        (c      "https://github.com/tree-sitter/tree-sitter-c" "v0.23.2")
        (cpp    "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.2")
        (bash   "https://github.com/tree-sitter/tree-sitter-bash")
        (rust   "https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2")
        (go     "https://github.com/tree-sitter/tree-sitter-go" "v0.21.2")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "v0.23.1")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx")
        ))


;; 记录已提示过的 treesit 语言（避免重复 message）
(defvar my/treesit-enabled-languages nil
  "Languages for which tree-sitter has been enabled and notified.")

(dolist (pair '((python-mode         . python-ts-mode)
                (c-mode              . c-ts-mode)
                (c++-mode            . c++-ts-mode)
                (sh-mode             . bash-ts-mode)
                (rust-mode           . rust-ts-mode)
                (go-mode             . go-ts-mode)
                (javascript-mode     . js-ts-mode)
                (typescript-mode     . typescript-ts-mode)
                (tsx-mode            . tsx-ts-mode)
                ))
  (let* ((from (car pair))
         (to   (cdr pair))
         ;; python-ts-mode -> python
         (lang (intern
                (string-remove-suffix
                 "-ts-mode"
                 (symbol-name to)))))
    (when (treesit-language-available-p lang)
      (add-to-list 'major-mode-remap-alist pair)
      (unless (memq lang my/treesit-enabled-languages)
        (push lang my/treesit-enabled-languages)
        (message "[treesit] Enabled tree-sitter for %s" lang)))))


;; disable treesit when large file
(defun my/disable-treesit-for-large-files ()
  "Disable treesit for large buffers."
  (when (> (buffer-size) (* 1 1024 1024)) ; 1MB
    (when (bound-and-true-p treesit-font-lock-mode)
      (treesit-font-lock-mode -1)
      (message "[treesit] Disabled for large file"))))

(add-hook 'find-file-hook #'my/disable-treesit-for-large-files)


(provide 'core-treesit)
