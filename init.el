;; Packages
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(setq package-enable-at-startup nil)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(cider
                      helm
                      projectile
                      helm-projectile
                      js2-mode
                      json-mode
                      ace-window
                      ace-jump-mode
                      monokai-theme
                      solarized-theme
                      swiper
                      rjsx-mode
                      magit
                      tide
                      company))

(dolist (p my-packages)
      (when (not (package-installed-p p))
          (package-install p)))

;; ENV/PATH
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "LANG" "en_US.UTF-8")
(setenv "NODE_NO_READLINE" "1") ;; Fixes NODE REPL from garbage
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Customization
(require 'uniquify)
(delete-selection-mode 1)
(setq-default cursor-type 'bar)
(setq-default indent-tabs-mode nil)
(global-auto-revert-mode)
(setq indent-tabs-mode nil
      standard-indent 2
      fill-column 85
      require-final-newline t
      column-number-mode t
      initial-scratch-message ""
      visible-bell nil
      kill-whole-line t
      ;; scroll
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1)
(setq ring-bell-function ;; Highlight status line on error
      (lambda()
        (invert-face 'mode-line)
        (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Shell
(setq comint-scroll-to-bottom-on-input t
      comint-get-old-input (lambda () "")
      comint-input-sender (lambda (proc command)
                            (cond ;; Check for clear command and execute it.
                             ((string-match "^[ \t]*clear[ \t]*$" command)
                              (comint-send-string proc "\n")
                              (erase-buffer))
                             (t (comint-simple-send proc command)))))

;; UI
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(set-frame-parameter nil 'fullscreen 'fullboth)
(show-paren-mode t)
(put 'narrow-to-region 'disabled nil)
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Projectile + helm
(setq projectile-enable-caching nil)
(setq projectile-completion-system 'helm
      helm-candidate-number-limit 1000)
(add-hook 'after-init-hook #'projectile-global-mode)
(helm-projectile-on)
(setq helm-google-suggest-search-url "http://www.google.com/search?source=ig&hl=en&rlz=1G1GGLQ_ENUS264&q=%s&btnI=I'm+Feeling+Lucky")

;; Cider
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-log-messages nil
      nrepl-hide-special-buffers t
      cider-show-error-buffer 'only-in-repl
      cider-repl-pop-to-buffer-on-connect nil
      cider-prompt-save-file-on-load nil)

(global-prettify-symbols-mode t)
(add-hook 'clojure-mode
            (lambda ()
              (push '("fn" . ?λ) prettify-symbols-alist)))

;; js2
(setq js2-strict-missing-semi-warning nil
      js2-missing-semi-one-line-override nil
      js2-strict-inconsistent-return-warning nil
      js2-basic-offset 2)
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
(add-hook 'js-mode-hook 'js2-minor-mode)

;; TypeScript
(global-company-mode)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))
(setq company-tooltip-align-annotations t)
(setq typescript-indent-level 2)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; ace
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(custom-set-faces
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :background "black" :height 2.0 :foreground "white")))))

;; swiper
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)

;; org-mode
(setq org-src-fontify-natively t)

;; File associations
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Custom functions
(defun date-battery ()
  (interactive)
  (message (replace-regexp-in-string "%" "%%" (delete ?\n (shell-command-to-string "date '+%H:%M %A %d %b ' && pmset -g batt | egrep -o '\\d{1,3}%'")))))

(defun prettier ()
  (interactive)
  (shell-command
   (format "$(npm bin)/prettier --write --no-semi --single-quote --no-bracket-spacing --print-width %s %s"
           (if (string-match "react-monorepo" (buffer-file-name)) "100" "85")
           (shell-quote-argument (buffer-file-name)))))

(setq is-theme-dark t)
(defun toggle-theme ()
  (interactive)
  (if is-theme-dark
      (load-theme 'monokai t)
      (load-theme 'solarized-light t))
  (setq is-theme-dark (not is-theme-dark)))
(toggle-theme)

(defun figwheel ()
  (interactive)
  (insert "(use 'figwheel-sidecar.repl-api)(cljs-repl)"))

;; Shortcuts
(global-set-key (kbd "<f7>") 'toggle-theme)
(global-set-key (kbd "<f1>") 'date-battery)
(global-set-key (kbd "<f2>") 'magit-status)
(global-set-key (kbd "<f4>") 'prettier)

(global-set-key (kbd "C-q") 'mark-sexp)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-c SPC") 'avy-goto-word-1)
(global-set-key (kbd "C-c g") 'helm-google-suggest)

(global-set-key (kbd "s-<down>")  'windmove-down)
(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>")    'windmove-up)
