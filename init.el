;; Packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(when (not package-archive-contents)
  (package-refresh-contents))


;; Customization
(setq-default indent-tabs-mode nil
              standard-indent 2
              make-backup-files nil
              column-number-mode t
              backup-directory-alist `((".*" . ,temporary-file-directory))
              auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;; UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(set-frame-parameter nil 'fullscreen 'fullboth)
(show-paren-mode t)
(display-time)


;; Projectile + helm
(setq projectile-completion-system 'helm
      projectile-enable-caching nil
      helm-candidate-number-limit 1000)
(add-hook 'after-init-hook #'projectile-global-mode)
(helm-projectile-on)
(helm-mode 1)


;; Cider
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages nil
      nrepl-hide-special-buffers t
      cider-show-error-buffer 'only-in-repl
      cider-prompt-save-file-on-load nil)


;; js2
(setq js2-strict-missing-semi-warning nil
      js2-missing-semi-one-line-override nil)
(add-hook 'js-mode-hook 'js2-minor-mode)


;; Custom functions
(defun toggle-theme ()
  "Toggle between light and dark themes. Different dark theme is used if emacs runs in terminal"
  (interactive)
  (cond ((and is-theme-dark (not window-system)) (load-theme 'wheatgrass t)
         (set-face-attribute 'helm-selection nil :background "#A9A9A9"))
        (is-theme-dark (load-theme 'solarized-dark t))
        ((not is-theme-dark) (load-theme 'solarized-light t)))
  (setq is-theme-dark (not is-theme-dark)))

(setq is-theme-dark (not window-system))
(toggle-theme)


;; Shortcuts
(global-set-key (kbd "<f7>") 'toggle-theme)
(global-set-key (kbd "C-q") 'mark-sexp)

