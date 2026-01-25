;;; ui-icons.el --- UI tweaks -*- lexical-binding: t -*-

;; Icons core
(use-package nerd-icons
  :ensure t)

(set-face-attribute 'default nil
                    :family "FiraCode Nerd Font"
                    :height 100)

;; Dired
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;; Completion
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook
            #'nerd-icons-completion-marginalia-setup))

;; Icons for completion
(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters
               #'nerd-icons-corfu-formatter))

(provide 'ui-icons)
