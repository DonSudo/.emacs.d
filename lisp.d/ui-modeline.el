;;; ui-modeline.el --- UI tweaks -*- lexical-binding: t -*-

(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; 显示函数到 modeline
(which-function-mode 1)
(setq which-func-unknown "N/A")

;; 显示项目到 modeline
;;(setopt project-mode-line t)

;; 更紧凑的 modeline
;;(setq mode-line-compact 'long)

;; 关闭不必要的 minor-modes 列表
;;(setq mode-line-modes nil)
(setq-default minor-mode-alist nil)

;; 高亮当前窗口的 modeline
(setq mode-line-highlight '((t (:inherit highlight))))


(provide 'ui-modeline)
