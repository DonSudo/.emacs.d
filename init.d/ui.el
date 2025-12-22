;;; ui.el --- UI tweaks -*- lexical-binding: t -*-

;; ------------------------------
;; Font setting
;; ------------------------------
(setq frame-resize-pixelwise t)

(defvar my-font-size (cond ((eq system-type 'darwin) 15)
                                 ((eq system-type 'windows-nt) 14)
                                 (t 16))
  "Current font size.")

(defvar my-font-weight "medium"
  "Current font weight.")

(defvar my-fonts
  `((default . ,(cond ((eq system-type 'darwin) "SF Mono")
                      ((eq system-type 'windows-nt) "Fira Code")
                      (t "Fira Code")))
    (fixed . ,(cond ((eq system-type 'darwin) "SF Mono")
                    ((eq system-type 'windows-nt) "Fira Code")
                    (t "Fira Code")))
    (fixed-serif . ,(cond ((eq system-type 'darwin) "New York")
                          ((eq system-type 'windows-nt) "Fira Code")
                          (t "Fira Code")))
    (variable . ,(cond ((eq system-type 'darwin) "SF Pro")
                       ((eq system-type 'windows-nt) "Fira Code")
                       (t "Fira Code")))
    (han . ,(cond ((eq system-type 'darwin) "PingFang SC")
                  ((eq system-type 'windows-nt) "方正柳公权楷书 简繁")
                  (t "方正柳公权楷书 简繁")))
    (cjk . ,(cond ((eq system-type 'darwin) "PingFang SC")
                  ((eq system-type 'windows-nt) "Microsoft Yahei")
                  (t "Noto Sans CJK SC")))
    (symbol . ,(cond ((eq system-type 'darwin) "Apple Color Emoji")
                     ((eq system-type 'windows-nt) "Segoe UI Emoji")
                     (t "Noto Color Emoji"))))
  "Fonts to use.")

(defun my--get-font-family (key)
  "Get font family with KEY."
  (let ((font (alist-get key my-fonts)))
    (if (string-empty-p font)
        (alist-get 'default my-fonts)
      font)))

(defun my-load-default-font ()
  "Load default font configuration."
  (let ((default-font (format "%s-%s:%s"
                              (my--get-font-family 'default)
                              my-font-size my-font-weight)))
    (add-to-list 'default-frame-alist (cons 'font default-font))))

(defun my-load-face-font ()
  "Load face font configuration."
  (let ((variable-font (format "%s-%s:%s" (my--get-font-family 'variable)
                               my-font-size my-font-weight))
        (fixed-font (format "%s-%s:%s" (my--get-font-family 'fixed)
                            my-font-size my-font-weight))
        (fixed-serif-font (format "%s-%s:%s" (my--get-font-family 'fixed-serif)
                                  my-font-size my-font-weight)))
    (set-face-attribute 'variable-pitch nil :font variable-font )
    (set-face-attribute 'fixed-pitch nil :font fixed-font)
    (set-face-attribute 'fixed-pitch-serif nil :font fixed-serif-font)
    (set-face-attribute 'mode-line-active nil :font variable-font)
    (set-face-attribute 'mode-line-inactive nil :font variable-font)))

(defun my-load-charset-font (&optional font)
  "Load charset FONT configuration."
  (let ((default-font (or font (format "%s-%s:%s"
                                       (my--get-font-family 'default)
                                       my-font-size my-font-weight)))
        (han-font (my--get-font-family 'han))
        (cjk-font (my--get-font-family 'cjk))
        (symbol-font (my--get-font-family 'symbol)))
    (set-frame-font default-font)
    (unless (eq system-type 'darwin)
      (add-to-list 'face-font-rescale-alist `(,han-font . 1.2))
      (add-to-list 'face-font-rescale-alist `(,cjk-font . 1.2)))
    (set-fontset-font t 'han han-font)
    (dolist (charset '(kana hangul cjk-misc bopomofo))
      (set-fontset-font t charset cjk-font))
    (set-fontset-font t 'symbol symbol-font)
    (set-fontset-font t 'unicode symbol-font nil 'append)))

(my-load-default-font)

;; Run after startup
(add-hook 'after-init-hook (lambda ()
                             (when (display-graphic-p)
                               (my-load-face-font)
                               (my-load-charset-font))))

;; ------------------------------
;; Theme
;;; ------------------------------
(use-package modus-themes
  :defer t
  :config
  (load-theme 'modus-operandi t))


;; ------------------------------
;; Modeline (simple)
;; ------------------------------
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)
;;(which-function-mode 1)
;;(setq which-func-unknown "N/A")

(setq mode-line-modes nil)

(with-eval-after-load 'evil
  (setq evil-normal-state-tag   " NORMAL "
        evil-insert-state-tag   " INSERT "
        evil-visual-state-tag   " VISUAL "
        evil-replace-state-tag  " REPLACE "
        evil-motion-state-tag   " MOTION "
        evil-emacs-state-tag    " EMACS "))

(custom-set-faces
 '(mode-line
   ((t (:height 0.95
        :background "#f2f2f2"
        :foreground "#444444"
        :box (:line-width 1 :color "#dddddd")))))
 '(mode-line-inactive
   ((t (:height 0.95
        :background "#fafafa"
        :foreground "#999999"
        :box (:line-width 1 :color "#eeeeee")))))

 ;; Evil states
 '(evil-normal-state ((t (:foreground "#005f87" :weight bold))))
 '(evil-insert-state ((t (:foreground "#5f8700" :weight bold))))
 '(evil-visual-state ((t (:foreground "#875f00" :weight bold))))
 '(evil-replace-state ((t (:foreground "#af0000" :weight bold))))
 '(evil-motion-state ((t (:foreground "#5f5faf" :weight bold))))
 '(evil-emacs-state  ((t (:foreground "#87005f" :weight bold)))))


;; ------------------------------
;; buffer tab
;; ------------------------------
;; (global-tab-line-mode 1)

;; ;; 只显示文件 buffer
;; (setq tab-line-tabs-function
;;       (lambda ()
;;         (let ((buffers (buffer-list)))
;;           (cl-remove-if-not
;;            (lambda (b) (buffer-file-name b))
;;            buffers))))

;; (global-set-key (kbd "C-<tab>") 'tab-next)
;; (global-set-key (kbd "C-S-<tab>") 'tab-previous)


;; ------------------------------
;; smooth scroll
;; ------------------------------
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)) ;; 每次滚动 1 行
      mouse-wheel-progressive-speed nil           ;; 关闭加速度
      scroll-step 1                               ;; 光标逐行滚动
      scroll-conservatively 101                   ;; 防止跳动
      scroll-margin 5)

(provide 'ui)
