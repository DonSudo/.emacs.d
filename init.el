;;; init.el -*- lexical-binding: t -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; ------------------------------
;; package.el
;; ------------------------------
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

;; ------------------------------
;; load init.d
;; ------------------------------
(add-to-list 'load-path (expand-file-name "lisp.d" user-emacs-directory))

(mapc #'require 
      '(core-init
        core-pkg
        core-misc
        core-treesit
        ui-core
        ui-theme
        ui-pop
        ui-icons
        lsp-eglot
        vim-core
        vim-integrate
        vim-key
        org-conf
        ;;org-note
        edit-cmpl
        edit-format
        ;; edit-snippet
        edit-tools))

;; Lanuch timer
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs 启动完成，耗时 %.3fs，加载了 %d 个库"
                     (float-time
                      (time-subtract after-init-time before-init-time))
                     (length load-path))))
