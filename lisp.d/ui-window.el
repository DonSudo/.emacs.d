;;; ui-window.el --- UI tweaks -*- lexical-binding: t -*-

;; Minimum window height
(setq window-min-height 1)

;; Vertical window divider
(setq window-divider-default-right-width 4)
(setq window-divider-default-bottom-width 4)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

;; emacs self window manage
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)

;; (add-to-list 'display-buffer-alist
;;              '((lambda (buffer _)
;;                  (not (get-buffer-window buffer)))
;;                display-buffer-in-direction
;;                (direction . right)))


(provide 'ui-window)
