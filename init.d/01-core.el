;;; core.el --- core settings -*- lexical-binding: t -*-

;; ------------------------------
;; Basic settings
;; ------------------------------
(setq ring-bell-function 'ignore
      use-dialog-box nil
      visible-bell nil
      make-backup-files nil
      auto-save-default nil
      use-short-answers t)

(setq-default indent-tabs-mode nil
              tab-width 4
              fill-column 80)

;; Line numbers globally
(global-display-line-numbers-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Save minibuffer history
(savehist-mode 1)

(setopt project-mode-line t)
(show-paren-mode 1)
