;;; ui.el --- UI tweaks -*- lexical-binding: t -*-

;; Font
(setq frame-resize-pixelwise t)
;; Font (defer after init)
(add-hook 'after-init-hook
          (lambda ()
            (when (display-graphic-p)
              (set-face-attribute 'default nil
                                  :family "Fira Code"
                                  :height 140))))

;; Theme
(use-package modus-themes
  :defer t
  :config
  (load-theme 'modus-operandi t))

;; Modeline (simple)
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; smooth scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; 每次滚动 1 行
      mouse-wheel-progressive-speed nil           ;; 关闭加速度
      scroll-step 1                               ;; 光标逐行滚动
      scroll-conservatively 101                   ;; 防止跳动
      scroll-margin 3)
