;;; ui-font.el --- UI tweaks -*- lexical-binding: t -*-

(defvar my-font-size (cond ((eq system-type 'darwin) 15)
                                 ((eq system-type 'windows-nt) 14)
                                 (t 14))
  "Current font size.")

(defvar my-font-weight "regular"
  "Current font weight.")

(defvar my-fonts
  `((default . ,(cond ((eq system-type 'darwin) "SF Mono")
                      ((eq system-type 'windows-nt) "FiraCode Nerd Font")
                      (t "Fira Code Nerd Font")))
    (fixed . ,(cond ((eq system-type 'darwin) "SF Mono")
                    ((eq system-type 'windows-nt) "FiraCode Nerd Font")
                    (t "Fira Code Nerd Font")))
    (fixed-serif . ,(cond ((eq system-type 'darwin) "New York")
                          ((eq system-type 'windows-nt) "FiraCode Nerd Font")
                          (t "Fira Code Nerd Font")))
    (variable . ,(cond ((eq system-type 'darwin) "SF Pro")
                       ((eq system-type 'windows-nt) "FiraCode Nerd Font")
                       (t "Fira Code Nerd Font")))
    (han . ,(cond ((eq system-type 'darwin) "PingFang SC")
                  ((eq system-type 'windows-nt) "方正柳公权楷书 简繁")
                  (t "LXGWWenKaiMono")))
    (cjk . ,(cond ((eq system-type 'darwin) "PingFang SC")
                  ((eq system-type 'windows-nt) "Microsoft Yahei")
                  (t "Noto Sans CJK SC")))
    (symbol . ,(cond ((eq system-type 'darwin) "Apple Color Emoji")
                     ((eq system-type 'windows-nt) "Symbols Nerd Font")
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


(provide 'ui-font)
