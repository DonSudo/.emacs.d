(use-package which-key
  :defer 1
  :init
  (setq which-key-idle-delay 0.5
        which-key-separator " → "
        which-key-min-display-lines 2)
  :config
  (which-key-mode 1))
