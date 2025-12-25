;;; vim.el -*- lexical-binding: t -*-

(use-package evil
  :init
  (setq evil-want-Y-yank-to-eol t
        evil-want-C-i-jump nil
        evil-disable-insert-state-bindings t
        evil-want-keybinding nil
        evil-want-integration t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(setq evil-ex-search-count t)

;; Leader Key：只在 normal / motion
(defvar my/leader-map (make-sparse-keymap))

(dolist (state '(normal motion visual))
  (evil-define-key state 'global (kbd "SPC") my/leader-map))

(with-eval-after-load 'evil
  ;; C-x → SPC x
  (define-key my/leader-map (kbd "x") ctl-x-map)

  ;; M-x → SPC X
  (define-key my/leader-map (kbd "X") #'execute-extended-command)

  ;; C-h -> SPC h
  (define-key my/leader-map (kbd "h") help-map))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements
    "SPC x" "C-x"
    "SPC X" "M-x"
    "SPC h" "C-h"))

;; 定义 "t" 为 toggle / folds submenu
(defvar my/leader-toggle-map (make-sparse-keymap)
  "Leader toggle submenu (folding, outline, etc).")
(define-key my/leader-map (kbd "t") my/leader-toggle-map)

;; hs-minor-mode 折叠/展开
(define-key my/leader-toggle-map (kbd "f") 'hs-hide-block)
(define-key my/leader-toggle-map (kbd "F") 'hs-show-block)
(define-key my/leader-toggle-map (kbd "a") 'hs-hide-all)
(define-key my/leader-toggle-map (kbd "A") 'hs-show-all)

;; swithc theme
(define-key my/leader-toggle-map (kbd "t") #'modus-themes-rotate)


;; file key
(defvar my/leader-file-map (make-sparse-keymap)
  "Leader 'f' keymap for file operations.")
(define-key my/leader-map (kbd "f") my/leader-file-map)

(define-key my/leader-file-map (kbd "f") 'find-file)       ;; SPC f f : 打开文件
(define-key my/leader-file-map (kbd "r") 'recentf-open-files) ;; SPC f r : 打开最近文件
(define-key my/leader-file-map (kbd "s") 'save-buffer)     ;; SPC f s : 保存
(define-key my/leader-file-map (kbd "S") 'write-file)      ;; SPC f S : 另存为
(define-key my/leader-file-map (kbd "d") 'delete-file)     ;; SPC f d : 删除当前文件

;; window operation
(defvar my/leader-window-map (make-sparse-keymap)
  "Leader 'w' keymap for window management.")
(define-key my/leader-map (kbd "w") my/leader-window-map)

(define-key my/leader-window-map (kbd "v") 'split-window-right)  ;; SPC w v : 垂直分割
(define-key my/leader-window-map (kbd "s") 'split-window-below)  ;; SPC w s : 水平分割
(define-key my/leader-window-map (kbd "d") 'delete-window)       ;; SPC w d : 关闭窗口
(define-key my/leader-window-map (kbd "o") 'delete-other-windows) ;; SPC w o : 仅保留当前窗口
(define-key my/leader-window-map (kbd "h") 'windmove-left)
(define-key my/leader-window-map (kbd "l") 'windmove-right)
(define-key my/leader-window-map (kbd "j") 'windmove-down)
(define-key my/leader-window-map (kbd "k") 'windmove-up)

;; buffer manage
(defvar my/leader-buffer-map (make-sparse-keymap)
  "Leader 'b' keymap for buffer management.")
(define-key my/leader-map (kbd "b") my/leader-buffer-map)

(define-key my/leader-buffer-map (kbd "b") 'switch-to-buffer)   ;; SPC b b : 切换 buffer
(define-key my/leader-buffer-map (kbd "k") 'kill-buffer)        ;; SPC b k : 关闭 buffer
(define-key my/leader-buffer-map (kbd "n") 'next-buffer)        ;; SPC b n : 下一个 buffer
(define-key my/leader-buffer-map (kbd "p") 'previous-buffer)    ;; SPC b p : 上一个 buffer

(defun my/new-empty-buffer()
  "Create and switch to a new empty buffer using text-mode"
  (interactive)
  (let ((buf (generate-new-buffer "*empty*")))
    (switch-to-buffer buf)
    (text-mode)))

(define-key my/leader-buffer-map (kbd "N") #'my/new-empty-buffer)    ;; SPC b p : 上一个 buffer


;; project manage
(defvar my/leader-project-map (make-sparse-keymap)
  "Leader 'p' keymap for project management.")
(define-key my/leader-map (kbd "p") my/leader-project-map)

(define-key my/leader-project-map (kbd "p") 'project-switch-project) ;; SPC p p : 切换项目
(define-key my/leader-project-map (kbd "f") 'project-find-file)      ;; SPC p f : 查找项目文件
(define-key my/leader-project-map (kbd "b") 'consult-project-buffer) ;; SPC p b : 项目 buffer
(define-key my/leader-project-map (kbd "s") 'consult-ripgrep)        ;; SPC p s : 项目搜索
(define-key my/leader-project-map (kbd "d") 'project-dired)          ;; SPC p d : 项目目录


;; search
(defvar my/leader-search-map (make-sparse-keymap)
  "Leader 's' keymap for search operations.")
(define-key my/leader-map (kbd "s") my/leader-search-map)

(define-key my/leader-search-map (kbd "s") 'consult-line)        ;; SPC s s : buffer 内搜索
(define-key my/leader-search-map (kbd "r") 'query-replace)       ;; SPC s r : 替换
(define-key my/leader-search-map (kbd "R") 'query-replace-regexp) ;; SPC s R : 正则替换


;; open tool
(defvar my/leader-open-map (make-sparse-keymap)
  "Leader 'o' keymap for opening utilities.")
(define-key my/leader-map (kbd "o") my/leader-open-map)

(define-key my/leader-open-map (kbd "f") 'recentf-open-files) ;; SPC o f : 最近文件
(define-key my/leader-open-map (kbd "e") 'eshell)            ;; SPC o e : 打开 eshell
(define-key my/leader-open-map (kbd "d") 'dired)             ;; SPC o d : 打开目录
(define-key my/leader-open-map (kbd "i") 'imenu)             ;; SPC o i : 跳转函数/变量
(define-key my/leader-open-map (kbd "c") 'calendar)          ;; SPC o c : 日历


(when (fboundp 'which-key-mode)
  (which-key-mode 1)
  (which-key-add-key-based-replacements
    "SPC f" "file"
    "SPC w" "window"
    "SPC b" "buffer"
    "SPC p" "project"
    "SPC s" "search"
    "SPC t" "toggle"
    "SPC o" "open"))

;; vim g- func
(evil-define-key 'normal 'global (kbd "gc") #'comment-line)
(evil-define-key 'visual 'global (kbd "gc") #'comment-dwim)
(evil-define-key 'normal 'global (kbd "gr") #'eglot-rename)

(evil-define-key 'normal 'global
  (kbd "gh") #'eldoc-box-help-at-point)

(defun my/evil-format ()
  (interactive)
  (cond
   ((and (bound-and-true-p eglot--managed-mode)
         (fboundp 'eglot-format))
    (eglot-format))
   (t
    (indent-region (region-beginning) (region-end)))))

(evil-define-key 'normal 'global
  (kbd "gq") #'my/evil-format)


;; [[ / ]] 跳转函数
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "[[") #'beginning-of-defun)
  (define-key evil-normal-state-map (kbd "]]") #'end-of-defun))

;; 代码折叠
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "za") #'outline-toggle-children)
  (define-key evil-normal-state-map (kbd "zc") #'outline-hide-subtree)
  (define-key evil-normal-state-map (kbd "zo") #'outline-show-subtree)
  (define-key evil-normal-state-map (kbd "zM") #'outline-hide-sublevels)
  (define-key evil-normal-state-map (kbd "zR") #'outline-show-all))


;; ensure minibuffer keep emacs mode
(with-eval-after-load 'evil
  (evil-set-initial-state 'minibuffer-mode 'emacs))

;; dired
(with-eval-after-load 'evil
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map
    (kbd "RET") #'dired-find-file
    (kbd "l")   #'dired-find-file
    (kbd "h")   #'dired-up-directory
    (kbd "q")   #'quit-window
    (kbd "g")   #'revert-buffer)))


(provide 'vim)
