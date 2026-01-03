;;; core-misc.el --- core settings -*- lexical-binding: t -*-

;; imenu
(setq imenu-auto-rescan t
      imenu-use-popup-menu nil)

;; 默认不处理从右到左排序的文本
;; 强制所有缓冲区默认使用从左到右的方向，避免自动检测
(setq-default bidi-paragraph-direction 'left-to-right)

;; 禁用双向括号算法，减少代码渲染压力
(setq-default bidi-inhibit-bpa t)

;; 如果是处理超大文件，可以在 hook 中局部禁用重新排序
(add-hook 'find-file-hook 
          (lambda () 
            (when (> (buffer-size) (* 10 1024 1024)) ; 大于10MB
              (setq bidi-display-reordering nil))))

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
