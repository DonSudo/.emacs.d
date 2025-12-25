;;; completion.el -*- lexical-binding: t -*-

;; ------------------------------
;; Orderless
;; ------------------------------
(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; ------------------------------
;; Vertico
;; ------------------------------
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t)
  (vertico-preselect 'first)
  (vertico-count 6))


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

(global-set-key (kbd "M-i") #'consult-imenu)
(global-set-key (kbd "M-o") #'consult-outline)

;; ------------------------------
;; Corfu (inline completion)
;; ------------------------------
;; 空格间隔的补全(minibuffer)
(use-package corfu
  :defer t
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-count 6)
  (corfu-preselect 'first)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-on-exact-match nil)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
  :hook
  (evil-insert-state-entry . corfu-mode)
  (evil-insert-state-exit  . (lambda () (corfu-mode -1))))

(provide 'cmpl)
