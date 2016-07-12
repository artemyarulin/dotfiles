;; PureScript REPL

(setq psci/arguments '("/usr/local/bin/psci"
                       "bower_components/*/src/**//*.purs"
                       "*.purs"
                       "response/**/*.purs"))

(defun refresh-repl ()
  (interactive)
  (let ((cur-ns (cur-ps-ns))
        (psci-buff (get-buffer "*psci*"))
        (cur-buff (current-buffer)))
    (psci/reset!)
    (set-buffer psci-buff)
    (insert (concat "import " cur-ns "\n"))
    (set-buffer cur-buff)))

(defun cur-ps-ns ()
  (save-excursion
    (goto-char (point-min))
    (let ((regexp "^module\\s-+\\\([a-zA-Z0-9\\\.]+\\\)\\b"))
      (search-forward-regexp regexp)
      (match-string 1))))

(define-key global-map (kbd "C-c C-r") 'refresh-repl)
