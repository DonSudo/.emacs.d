;;; early-init.el --- Loaded before init.el  -*- lexical-binding: t; -*-

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
        (height . 80)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (internal-border-width . 24)
        (left-fringe    . 8)
        (right-fringe   . 8)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (undecorated . nil)
        ;;(alpha . 95)
        )) ;; 半透明，可选

(setq default-frame-alist initial-frame-alist)
(setq frame-inhibit-implied-resize t) ;; 防止 frame resize 阻塞启动

;; No message in scratch buffer
(setq initial-scratch-message nil)

;; Initial buffer
(setq initial-buffer-choice nil)

;; No file dialog
(setq use-file-dialog nil)

;; No dialog box
(setq use-dialog-box nil)

;; No popup windows
(setq pop-up-windows nil)


;; 延迟 GC，提升启动性能
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024))))

;; 禁止默认包初始化阻塞启动
(setq package-enable-at-startup nil)

;; 文本优化
(setq-default bidi-paragraph-direction 'left-to-right
              ;; 默认不进行双向重排
              bidi-display-reordering nil
              ;; 禁用双向括号算法, 阿拉伯语、希伯来语等从右向左的语言才需要
              bidi-inhibit-bpa t
              ;; 滚动优化, 滚动时不再精确计算每一行的属性
              fast-but-imprecise-scrolling t
              ;; 延迟语法高亮，仅在空闲时渲染
              jit-lock-defer-time 0)

(let ((tools "C:/Program Files/Git/usr/bin")) ;; 或者你自己的 diff 目录
  (add-to-list 'exec-path tools))

;; ~/.emacs.d/tools 作为外部工具优先路径
(let ((tools-dir (expand-file-name "~/.emacs.d/tools")))
  ;; 1. Emacs 查找可执行文件用
  (add-to-list 'exec-path tools-dir)

  ;; 2. Emacs 子进程继承的 PATH
  (setenv "PATH" (concat tools-dir path-separator (getenv "PATH"))))


(provide 'early-init)
