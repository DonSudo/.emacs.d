;;; early-init.el --- Loaded before init.el  -*- lexical-binding: t; -*-

;; ----------------------------
;; 1. 禁用 GUI 元素，避免闪烁
;; ----------------------------
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
        (height . 40)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (undecorated . nil)
        ;;(alpha . 95)
        )) ;; 半透明，可选

(setq default-frame-alist initial-frame-alist)
(setq frame-inhibit-implied-resize t) ;; 防止 frame resize 阻塞启动


;; ----------------------------
;; 3. 禁止启动信息
;; ----------------------------
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      inhibit-compacting-font-caches t)


;; ----------------------------
;; 4. 延迟 GC，提升启动性能
;; ----------------------------
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)


;; ----------------------------
;; 5. recentf 延迟加载
;; ----------------------------
(setq recentf-load-file nil)
(setq recentf-auto-cleanup 'never)


;; ----------------------------
;; 6. 禁止默认包初始化阻塞启动
;; ----------------------------
(setq package-enable-at-startup nil)


;; ----------------------------
;; 7. 提前禁用垃圾信息
;; ----------------------------
;;(setq message-log-max nil) ;; 启动阶段不输出多余日志

