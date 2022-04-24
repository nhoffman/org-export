;; emacs --batch --kill --load minimal-export.el -- infile.org
;; used to test minimal batch export

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
  (setq org-babel-python-command "python3")
  (setq python-indent-offset 4)
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-guess-indent-offset t)
  (setq python-indent-guess-indent-offset-verbose nil)

  (org-html-export-as-html)
  (write-file outfile))
