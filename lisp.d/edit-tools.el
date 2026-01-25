;;; edit-tools.el -*- lexical-binding: t -*-

;; symbol highlight
(use-package symbol-overlay
  :defer t
  :hook ((prog-mode . symbol-overlay-mode))
  :config
  ;; 高亮光标下 symbol
  (setq symbol-overlay-idle-time 0.3))


;; move line like VS Code
(use-package move-dup
  :bind
  ([S-M-down] . move-dup-duplicate-down)
  ([S-M-up] . move-dup-duplicate-up)
  ([M-up] . move-dup-move-lines-up)
  ([M-down] . move-dup-move-lines-down))


(provide 'edit-tools)
