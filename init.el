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
                      ace-window
                      ace-jump-mode
                      monokai-theme
                      solarized-theme
                      swiper
                      shell-pop))

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
(setq indent-tabs-mode nil
      standard-indent 2
      fill-column 120
      require-final-newline t
      make-backup-files nil
      column-number-mode t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      initial-scratch-message ""
      visible-bell nil
      kill-whole-line t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq ring-bell-function ;; Highlight status line on error
      (lambda()
        (invert-face 'mode-line)
        (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Shell + shell-pop
(setq comint-scroll-to-bottom-on-input t
      comint-get-old-input (lambda () "")
      comint-input-sender (lambda (proc command)
                            (cond ;; Check for clear command and execute it.
                             ((string-match "^[ \t]*clear[ \t]*$" command)
                              (comint-send-string proc "\n")
                              (erase-buffer))
                             (t (comint-simple-send proc command)))))
(custom-set-variables
 '(shell-pop-default-directory "/Users/fessguid/Projects/nopomore")
 '(shell-pop-autocd-to-working-dir nil)
 '(shell-pop-full-span t)
 '(shell-pop-shell-type (quote ("shell" "*shell*" (lambda nil (shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "<f9>") ;; CAPSLOCK remapped to it
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 40))

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
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm
      helm-candidate-number-limit 1000)
(add-hook 'after-init-hook #'projectile-global-mode)
(helm-projectile-on)
(helm-mode 1)
(setq helm-google-suggest-search-url "http://www.google.com/search?source=ig&hl=en&rlz=1G1GGLQ_ENUS264&q=%s&btnI=I'm+Feeling+Lucky")
(global-set-key (kbd "C-c g") 'helm-google-suggest)

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
      js2-missing-semi-one-line-override nil)
(add-hook 'js-mode-hook 'js2-minor-mode)

;; ace
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(custom-set-faces '(aw-leading-char-face
   ((t (:inherit ace-jump-face-foreground
                 :background "black"
                 :height 2.0
                 :foreground "white")))))

;; swiper
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)


;; Custom functions
(defun date-battery ()
  (interactive)
  (message (replace-regexp-in-string "%" "%%" (delete ?\n (shell-command-to-string "date '+%H:%M %A %d %b ' && pmset -g batt | egrep -o '\\d{1,3}%'")))))

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

(global-set-key (kbd "C-q") 'mark-sexp)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-c SPC") 'avy-goto-word-1)

(global-set-key (kbd "s-<down>")  'windmove-down)
(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>")    'windmove-up)
