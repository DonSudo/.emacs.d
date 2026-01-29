;;; core-misc.el --- core settings -*- lexical-binding: t -*-

;; Code format 
;; ---------- 全局 ----------
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default standard-indent 4)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local tab-width 4)
            (setq-local indent-tabs-mode nil)))

;; ---------- C 系 ----------
(setq-default c-basic-offset 4)

;; ---------- JS/TS ----------
(setq js-indent-level 4)
(setq typescript-indent-level 4)

;; ---------- Python ----------
(setq python-indent-offset 4)

;; ---------- treesit ----------
(setq-default
 c-ts-mode-indent-offset 4
 c++-ts-mode-indent-offset 4
 js-ts-indent-offset 4
 typescript-ts-mode-indent-offset 4
 python-ts-mode-indent-offset 4)

;; 开启全局视觉折行
(global-visual-line-mode 1)

;; 允许在没有空格的中文字符间折行 (Emacs 28+)
(setq word-wrap-by-category t)

;; 视觉折行时，不要切断单词（保持英文单词完整）
(setq-default word-wrap t)

;; Big file
(defun my/shutup-emacs-for-big-files ()
  (when (> (buffer-size) (* 10 1024 1024))
    (setq font-lock-mode nil)
    ;; 设为只读保护性能
    (setq-local buffer-read-only t)
    (setq-local truncate-lines t)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my/shutup-emacs-for-big-files)

;; imenu
(setq imenu-auto-rescan t
      imenu-use-popup-menu nil)

;; code folding
;; 只在 prog-mode 启用折叠
(add-hook 'prog-mode-hook #'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(add-hook 'markdown-mode-hook #'outline-minor-mode)
(add-hook 'org-mode-hook #'outline-minor-mode)

;; 折叠时不隐藏当前行
(setq outline-blank-line t)

;; 搜索时自动展开折叠
(setq hs-isearch-open t)


(provide 'core-misc)
