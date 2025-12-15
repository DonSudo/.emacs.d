;;; completion.el -*- lexical-binding: t -*-

;; Orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; ------------------------------
;; Vertico
;; ------------------------------
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

;; ------------------------------
;; Marginalia (annotations)
;; ------------------------------
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode 1))

;; ------------------------------
;; Consult (navigation / search)
;; ------------------------------
(use-package consult
  :defer t
  :init
  (setq consult-preview-key "M-."))


;; ------------------------------
;; Corfu (inline completion)
;; ------------------------------
(use-package corfu
  :defer t
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (add-hook 'evil-insert-state-entry-hook
            (lambda () (corfu-mode 1)))
  (add-hook 'evil-insert-state-exit-hook
            (lambda () (corfu-mode -1))))

(defun my/enable-corfu-safe ()
  "Enable Corfu safely: LSP modes use LSP CAPF, others use built-in CAPF."
  (cond
   ((member major-mode my/lsp-modes)
    (if (and (boundp 'eglot--current-server)
             (processp eglot--current-server))
        (corfu-mode 1)
      (unless (bound-and-true-p my/corfu-no-lsp-msg-shown)
        (message "[Corfu] LSP server not found, completion disabled")
        (setq-local my/corfu-no-lsp-msg-shown t)
        (corfu-mode -1))))
   (t
    ;; 非 LSP 模式，直接使用内置 CAPF
    (corfu-mode 1))))

(add-hook 'evil-insert-state-entry-hook #'my/enable-corfu-safe)
(add-hook 'evil-insert-state-exit-hook (lambda () (corfu-mode -1)))
