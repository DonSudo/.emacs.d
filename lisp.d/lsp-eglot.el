;;; lsp-eglot.el -*- lexical-binding: t -*-

;; eglot
(use-package eglot
  :defer t
  :hook
  ((python-ts-mode . eglot-ensure)
   (c-ts-mode      . eglot-ensure)
   (c++-ts-mode    . eglot-ensure))
  :init
  (setq eglot-report-progress nil
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.3
        eglot-code-action-indications '(mode-line)
        eglot-events-buffer-size 0)
  :config
  ;; 不接管 imenu(用 treesit + consult)
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
