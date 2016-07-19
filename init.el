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
                      swiper))

(dolist (p my-packages)
      (when (not (package-installed-p p))
          (package-install p)))

;; PATH
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Customization
(require 'uniquify)
(delete-selection-mode 1)
(setq indent-tabs-mode nil
      cursor-type 'bar
      standard-indent 2
      make-backup-files nil
      column-number-mode t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      initial-scratch-message ""
      visible-bell t)

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
(setq projectile-completion-system 'helm
      projectile-enable-caching nil
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

(defun figwheel ()
  (interactive)
  (insert "(use 'figwheel-sidecar.repl-api)(cljs-repl)"))

(defun cljc-test (ns)
  (interactive "sTest namespace:")
  (insert (concat "(ns " ns "-test\n"
		  "  (:require #?(:clj [clojure.test :refer [is are deftest]]\n"
		  "               :cljs [cljs.test :refer-macros [is are deftest async]])\n"
		  "            [" ns " :refer []]))"))
  (backward-char 4))

;; js2
(setq js2-strict-missing-semi-warning nil
      js2-missing-semi-one-line-override nil)
(custom-set-variables '(js2-basic-offset 2)
                      '(js2-bounce-indent-p nil))
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
(defun toggle-theme ()
  (interactive)
  (if is-theme-dark
      (load-theme 'monokai t)
      (load-theme 'solarized-light t))
  (setq is-theme-dark (not is-theme-dark)))

(setq is-theme-dark t)
(toggle-theme)

;; Shortcuts
(global-set-key (kbd "<f7>") 'toggle-theme)

(global-set-key (kbd "C-q") 'mark-sexp)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "C-c SPC") 'avy-goto-word-1)

(global-set-key (kbd "s-<down>")  'windmove-down)
(global-set-key (kbd "s-<left>")  'windmove-left)
(global-set-key (kbd "s-<right>") 'windmove-right)
(global-set-key (kbd "s-<up>")    'windmove-up)

(global-set-key (kbd "M-\\") 'just-one-space)
