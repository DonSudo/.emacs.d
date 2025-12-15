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
(add-to-list 'load-path (expand-file-name "init.d" user-emacs-directory))
;;(add-to-list 'load-path (expand-file-name "init.d/leader" user-emacs-directory))

(dolist (file '("01-core"
                "02-ui"
                "03-evil"
                "04-cmpl"
                "05-lsp"
                "06-tools"
                "07-org"
                "08-lang"
                "09-misc"))
  (load file nil 'nomessage))

;; recovery gc
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 128 1024 1024))))

;; Lanuch timer
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs 启动完成，耗时 %.3fs，加载了 %d 个库"
                     (float-time
                      (time-subtract after-init-time before-init-time))
                      (length load-path))))
