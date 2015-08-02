;; Packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(when (not package-archive-contents)
  (package-refresh-contents))


;; Customization
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "C-q") 'mark-sexp)


;; UI
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(set-frame-parameter nil 'fullscreen 'fullboth)
(custom-set-variables
 ;; For terminial the best is wheatgrass
 ;; Could't find a way to load it for terminal only
 '(custom-enabled-themes (quote (solarized-light)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))


;; Projectile + helm
(add-hook 'after-init-hook #'projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching nil)
(helm-projectile-on)
(helm-mode 1)
(setq helm-candidate-number-limit 1000)


;; cider
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages nil)
(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer 'only-in-repl)
(setq cider-prompt-save-file-on-load nil)


;; js2
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)
(add-hook 'js-mode-hook 'js2-minor-mode)
