;;; ui-tabbar.el --- UI tweaks -*- lexical-binding: t -*-

(tab-bar-mode t)

(setq tab-bar-close-button-show nil
      tab-bar-new-button-show nil
      tab-bar-tab-name-truncated-max 24
      tab-bar-history-mode nil
      tab-bar-hints t)

(setq tab-bar-format
      '(tab-bar-format-tabs
        tab-bar-separator
        tab-bar-format-align-right
        tab-bar-format-global))

(setq tab-bar-tab-name-function
      (lambda ()
        (let ((proj (project-current)))
          (if proj
              (file-name-nondirectory
               (directory-file-name
                (project-root proj)))
            (buffer-name)))))

(defun my/tab-bar-switch-to-project ()
  "Switch to a tab named after current project, or create it."
  (when-let* ((proj (project-current))
              (root (project-root proj))
              (name (file-name-nondirectory
                     (directory-file-name root))))
    (unless (member name (mapcar #'car (tab-bar-tabs)))
      (tab-bar-new-tab))
    (tab-bar-rename-tab name)))

(add-hook 'project-switch-project-hook
          #'my/tab-bar-switch-to-project)


(provide 'ui-tabbar)
