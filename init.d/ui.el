;;; ui.el --- UI tweaks -*- lexical-binding: t -*-

;; ------------------------------
;; Font setting
;; ------------------------------
(setq frame-resize-pixelwise t)

(defvar my-font-size (cond ((eq system-type 'darwin) 15)
                                 ((eq system-type 'windows-nt) 14)
                                 (t 14))
  "Current font size.")

(defvar my-font-weight "regular"
  "Current font weight.")

(defvar my-fonts
  `((default . ,(cond ((eq system-type 'darwin) "SF Mono")
                      ((eq system-type 'windows-nt) "Fira Code")
                      (t "Fira Code Nerd Font")))
    (fixed . ,(cond ((eq system-type 'darwin) "SF Mono")
                    ((eq system-type 'windows-nt) "Fira Code")
                    (t "Fira Code Nerd Font")))
    (fixed-serif . ,(cond ((eq system-type 'darwin) "New York")
                          ((eq system-type 'windows-nt) "Fira Code")
                          (t "Fira Code Nerd Font")))
    (variable . ,(cond ((eq system-type 'darwin) "SF Pro")
                       ((eq system-type 'windows-nt) "Fira Code")
                       (t "Fira Code Nerd Font")))
    (han . ,(cond ((eq system-type 'darwin) "PingFang SC")
                  ((eq system-type 'windows-nt) "方正柳公权楷书 简繁")
                  (t "LXGWWenKaiMono")))
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
(use-package ef-themes
  :ensure t
  :init
  (modus-themes-include-derivatives-mode 1)
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-random))

;;(defvar my/light-theme 'modus-operandi)
;;(defvar my/dark-theme 'modus-vivendi-tinted)
;;
;;(defun my/load-theme(theme)
;;  (mapc #'disable-theme custom-enabled-themes)
;;  (load-theme theme t))
;;
;;(defun my/toggle-theme()
;;  (interactive)
;;  (if (member my/dark-theme custom-enabled-themes)
;;      (my/load-theme my/light-theme)
;;    (my/load-theme my/dark-theme)))


;; ------------------------------
;; Modeline (simple)
;; ------------------------------
(line-number-mode 1)
(column-number-mode 1)
(size-indication-mode 1)

;; 显示函数到 modeline
;;(which-function-mode 1)
;;(setq which-func-unknown "N/A")

;; 显示项目到 modeline
(setopt project-mode-line t)

;;(setq mode-line-modes nil)
(setq-default minor-mode-alist nil)
(setq mode-line-right-align-edge 'right-margin)


;; ------------------------------
;; buffer tab (无必要)
;; ------------------------------
(global-tab-line-mode 1)

(setq tab-line-close-button-show nil
      tab-line-new-button-show nil)

(tab-line-switch-to-next-tab)
(tab-line-switch-to-prev-tab)


;; ------------------------------
;; tab bar
;; ------------------------------
(tab-bar-mode t)

(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-min-width 12
      tab-bar-max-width 30
      tab-bar-tab-name-truncated-max 24
      tab-bar-history-mode nil)

(setq tab-bar-format
      '(tab-bar-format-tabs
        tab-bar-separator
        tab-bar-format-align-right
        tab-bar-format-global))

;;(defun my/project-name()
;;  (when-let ((proj (project-current)))
;;    (file-name-nondirectory
;;     (directory-file-name
;;      (project-root proj)))))
;;
;;(defun my/tab-exists-p (name)
;;  (member name (mapcar (lambda (tab) (alist-get 'name tab))
;;                       (tab-bar-tabs))))
;;
;;(defun my/switch-to-project-tab()
;;  (when-let ((name (my/project-name)))
;;    (unless (my/tab-exists-p name)
;;      (tab-bar-new-tab)
;;      (tab-bar-rename-tab name))
;;    (tab-bar-select-tab-by-name name)))
;;
;;(add-hook 'project-find-functions
;;          (lambda (_dir)
;;            (my/switch-to-project-tab)
;;            nil))

;; ------------------------------
;; smooth scroll
;; ------------------------------
(setq mouse-wheel-scroll-amount '(2 ((shift) . 5)) ;; 每次滚动 1 行
      mouse-wheel-progressive-speed nil           ;; 关闭加速度
      scroll-step 1                               ;; 光标逐行滚动
      scroll-conservatively 101                   ;; 防止跳动
      scroll-margin 5)

(provide 'ui)
