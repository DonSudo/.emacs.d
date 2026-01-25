;;; ui-pop.el --- UI tweaks -*- lexical-binding: t -*-

(setq eldoc-echo-area-use-multiline-p nil)
(setq eldoc-documentation-strategy 'eldoc-documentation-default)

(use-package eldoc-box
  :defer t
  :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-max-pixel-height 300))

;;(setq flymake-show-diagnostics-at-end-of-line t)

(use-package popper
  :defer t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Help\\*"
          "\\*Compile-Log\\*"
          "\\*Flymake diagnostics\\*"
          "\\*eldoc\\*"))
  (popper-mode 1)
  (popper-echo-mode 1))

(provide 'ui-pop)
