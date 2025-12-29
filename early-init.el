;;; early-init.el --- Loaded before init.el  -*- lexical-binding: t; -*-

;; ----------------------------
;; 1. 禁用 GUI 元素，避免闪烁
;; ----------------------------
(setq inhibit-x-resources t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-compacting-font-caches t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10) ;; 左右边距


;; ----------------------------
;; 2. Frame 默认参数，避免启动闪烁 / resize
;; ----------------------------
(setq initial-frame-alist
      '((width . 120)
        (height . 90)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (internal-border-width . 2)
        (left-fringe    . 1)
        (right-fringe   . 1)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (undecorated . nil)
        ;;(alpha . 95)
        )) ;; 半透明，可选

;;(unless (eq system-type 'windows-nt)
;;  (setq undecorated t))

(setq default-frame-alist initial-frame-alist)
(setq frame-inhibit-implied-resize t) ;; 防止 frame resize 阻塞启动


;; ----------------------------
;; 3. 降低启动损耗
;; ----------------------------
;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No frame title
(setq frame-title-format '("%b"))

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)

;; 关闭文件名特殊处理
(setq file-name-handler-alist nil)

;; 仅识别 git 做版本控制
(setq vc-handled-backends '(Git))

;; ----------------------------
;; 4. 延迟 GC，提升启动性能
;; ----------------------------
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.7)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024))))


;; ----------------------------
;; 5. recentf 延迟加载
;; ----------------------------
;;(setq recentf-load-file nil)
;;(setq recentf-auto-cleanup 'never)


;; ----------------------------
;; 6. 禁止默认包初始化阻塞启动
;; ----------------------------
(setq package-enable-at-startup nil)


(provide 'early-init)
