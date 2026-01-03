;;; core-treesit.el -*- lexical-binding: t -*-

(require 'treesit)
(setq treesit-font-lock-level 4)

;; grammar 动态库搜索路径
(setq treesit-extra-load-path
      (list (expand-file-name "tree-sitter" user-emacs-directory)))

(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (c      "https://github.com/tree-sitter/tree-sitter-c")
        (cpp    "https://github.com/tree-sitter/tree-sitter-cpp")
        (json   "https://github.com/tree-sitter/tree-sitter-json")
        (yaml   "https://github.com/ikatyang/tree-sitter-yaml")
        (bash   "https://github.com/tree-sitter/tree-sitter-bash")))

;; 只在 grammar 可用时 remap
;;(dolist (pair '((python-mode . python-ts-mode)
;;                (c-mode      . c-ts-mode)
;;                (c++-mode    . c++-ts-mode)
;;                (json-mode   . json-ts-mode)
;;                (yaml-mode   . yaml-ts-mode)
;;                (sh-mode     . bash-ts-mode)))
;;  (when (treesit-language-available-p (car pair))
;;    (add-to-list 'major-mode-remap-alist pair)))

;; 记录已提示过的 treesit 语言（避免重复 message）
(defvar my/treesit-enabled-languages nil
  "Languages for which tree-sitter has been enabled and notified.")

(dolist (pair '((python-mode . python-ts-mode)
                (c-mode      . c-ts-mode)
                (c++-mode    . c++-ts-mode)
                (json-mode   . json-ts-mode)
                (yaml-mode   . yaml-ts-mode)
                (sh-mode     . bash-ts-mode)))
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
