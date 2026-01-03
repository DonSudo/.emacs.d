;;; ui-modeline.el --- UI tweaks -*- lexical-binding: t -*-

(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; 显示函数到 modeline
(which-function-mode 1)
(setq which-func-unknown "N/A")

;; 显示项目到 modeline
;;(setopt project-mode-line t)

;;(setq mode-line-modes nil)
(setq-default minor-mode-alist nil)
(setq mode-line-right-align-edge 'right-margin)


(provide 'ui-modeline)
