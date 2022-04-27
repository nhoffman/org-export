;; emacs --batch --kill --load minimal-export.el -- infile.org
;; used to test minimal batch export

;; (defun advised-python-shell-make-comint (orig-fun &rest args)
;;   ;; (message "--> Called with args: %S" args)
;;   (setq args (append '("python3 -q") (cdr args)))
;;   ;; (message "--> Modified args: %S" args)
;;   (let ((res (apply orig-fun args)))
;;     ;; (message "--> Result: %S" res)
;;     res))

(defun advised-python-shell-make-comint (orig-fun &rest args)
  (setq args (append '("python3 -q") (cdr args)))
  (apply orig-fun args))

(advice-add 'python-shell-make-comint :around #'advised-python-shell-make-comint)

(let* ((infile (car (last command-line-args)))
       (outfile (concat (file-name-sans-extension infile) ".html")))

  (find-file infile)
  (org-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)))

  (setq org-confirm-babel-evaluate nil)
  (setq python-indent-offset 4)
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters "python3")

  (org-html-export-as-html)
  (write-file outfile))
