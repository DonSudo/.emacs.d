;;; lsp-eglot.el -*- lexical-binding: t -*-

;; Eglot local LSP server path
(defconst my/lsp-server-dir
  (expand-file-name "lsp-server" user-emacs-directory)
  "Directory containing local LSP servers.")

(when (file-directory-p my/lsp-server-dir)
  ;; Emacs 内部查找
  (add-to-list 'exec-path my/lsp-server-dir)
  ;; 子进程 / eglot / jsonrpc 查找
  (setenv "PATH"
          (concat my/lsp-server-dir
                  path-separator
                  (getenv "PATH"))))

;; Eglot
(use-package eglot
  :defer t
  :hook
  ((python-ts-mode . eglot-ensure)
   (c-ts-mode      . eglot-ensure)
   (c++-ts-mode    . eglot-ensure)
   (rust-ts-mode   . eglot-ensure)
   (go-ts-mode     . eglot-ensure)
   (lua-ts-mode    . eglot-ensure))
  :init
  (setq eglot-report-progress nil
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.3
        eglot-code-action-indications '(mode-line)
        eglot-events-buffer-size 0)
  :config
  ;; 默认不接管所有 mode
  (setq eglot-extend-to-xref t)
  ;; 不要在 org / text 里乱启动
  (add-to-list 'eglot-ignored-server-capabilities :documentFormattingProvider))


(defun my/eglot-should-start-p ()
  "Return non-nil if eglot should start in current buffer."
  (and (project-current)
       (not (derived-mode-p
             'emacs-lisp-mode
             'org-mode
             'markdown-mode))))

(defun my/eglot-auto-start ()
  (when (my/eglot-should-start-p)
    (eglot-ensure)))

(add-hook 'prog-mode-hook #'my/eglot-auto-start)


(provide 'lsp-eglot)
