;;; core.el --- core settings -*- lexical-binding: t -*-

;; ------------------------------
;; Basic settings
;; ------------------------------
(setq ring-bell-function 'ignore
      use-dialog-box nil
      visible-bell nil
      make-backup-files nil
      auto-save-default nil
      use-short-answers t)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; Line numbers globally
(global-display-line-numbers-mode 1)

;; Highlight current line
;;(global-hl-line-mode 1)

;; Save minibuffer history
(savehist-mode 1)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Minimum window height
(setq window-min-height 1)

;; Vertical window divider
(setq window-divider-default-right-width 24)
(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; 将下划线绘制在下降线（位置较低）
(setq x-underline-at-descent-line t)

;; 显示项目到 modeline
(setopt project-mode-line t)

;; 自动补全成对字符
(electric-pair-mode 1)

;; 自动高亮成对括号
(show-paren-mode 1)

;; 自动刷新 buffe
(global-auto-revert-mode 1)

;; 输入替换选中区域
(delete-selection-mode 1)

;; 搜索计数器
(setq isearch-lazy-count t)

;; 启用后台编译
(setq native-comp-deferred-compilation t)


;; Buffer 编码
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Recentf
(setq recentf-max-saved-items 100)
(add-hook 'after-init-hook #'recentf-mode)


(provide 'core)
