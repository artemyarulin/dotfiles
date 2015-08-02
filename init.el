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


;; UI
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(set-frame-parameter nil 'fullscreen 'fullboth)


;; Projectile + helm
(add-hook 'after-init-hook #'projectile-global-mode)
(setq projectile-completion-system 'helm)
(setq projectile-enable-caching nil)
(helm-projectile-on)
(helm-mode 1)
(setq helm-candidate-number-limit 1000)


;; Cider
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages nil)
(setq nrepl-hide-special-buffers t)
(setq cider-show-error-buffer 'only-in-repl)
(setq cider-prompt-save-file-on-load nil)


;; js2
(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)
(add-hook 'js-mode-hook 'js2-minor-mode)


;; Custom functions
(setq is-term (getenv "TMUX"))

(defun toggle-theme ()
  "Toggle between light and dark themes. Different dark theme is used if emacs runs in terminal"
  (interactive)
  (cond ((and is-theme-dark is-term) (load-theme 'wheatgrass t)
                                     (set-face-attribute 'helm-selection nil :background "#A9A9A9"))
        (is-theme-dark (load-theme 'solarized-dark t))
        ((not is-theme-dark) (load-theme 'solarized-light t)))
  (setq is-theme-dark (not is-theme-dark)))

(setq is-theme-dark (when is-term t))
(toggle-theme)


;; Shortcuts
(global-set-key (kbd "<f7>") 'toggle-theme)
(global-set-key (kbd "C-q") 'mark-sexp)

