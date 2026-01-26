;;; core-init.el --- core settings -*- lexical-binding: t -*-

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

;; Save minibuffer history
(savehist-mode 1)

;; quicker repeat
;;(repeat-mode 1)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; 长行保护
(global-so-long-mode 1)        

;; 关闭文件名特殊处理
(setq file-name-handler-alist nil)

;; 仅识别 git 做版本控制
(setq vc-handled-backends '(Git))

;; 内置的垂直补全交互
;;(fido-vertical-mode 1)


;; ------------------------------
;; core UI
;; ------------------------------
;; No frame title
(setq frame-title-format '("%b"))

;; Line numbers globally
(global-display-line-numbers-mode 1)
;; relative number
(setq display-line-numbers-type 'relative)
;; these mode disable line number
(dolist (mode '(term-mode
                shell-mode
                eshell-mode
                vterm-mode
                treemacs-mode
                ;;org-mode
                ))
  (add-hook (intern (format "%s-hook" mode))
            (lambda () (display-line-numbers-mode 0))))

;; 自动高亮成对括号
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; 将下划线绘制在下降线（位置较低）
(setq x-underline-at-descent-line t)

;; smooth scroll
(pixel-scroll-precision-mode 1)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)) ;; 每次滚动 1 行
      mouse-wheel-progressive-speed nil           ;; 关闭加速度
      scroll-step 1                               ;; 光标逐行滚动
      scroll-conservatively 101                   ;; 防止跳动
      scroll-margin 5)


;; ------------------------------
;; Buffer 编码
;; ------------------------------
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)


;; 自动补全成对字符
(electric-pair-mode 1)

;; 自动刷新 buffer
(global-auto-revert-mode 1)

;; 输入替换选中区域
(delete-selection-mode 1)

;; 搜索计数器
(setq isearch-lazy-count t)

;; 启用后台编译
(setq native-comp-deferred-compilation t)

;; Recent files
(setq recentf-max-saved-items 100)

;; Dired 
(setq-default dired-dwim-target t)

(provide 'core-init)
