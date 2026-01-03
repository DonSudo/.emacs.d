;;; ui-theme.el --- UI tweaks -*- lexical-binding: t -*-

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


(provide 'ui-theme)
