(unless (>= emacs-major-version 26)
  (error "Error: emacs version 26 or greater is required"))

(provide 'cli)

;; configuration applying to all commands
(setq lexical-binding t)
(setq make-backup-files nil)
(setq debug-on-error t)

(defun cli-path-join (&rest x)
  "Join elements of x with a path separator and apply `expand-file-name'"
  (expand-file-name
   (concat
    (mapconcat 'file-name-as-directory (seq-take x (- (length x) 1)) "")
    (elt x (- (length x) 1)))))

(defvar cli-package-dir
  (cli-path-join
   (or (getenv "XDG_DATA_HOME") "~/.local/share") "org-export")
  "Location to install org-mode and dependencies")

(defvar cli-config-dir
  (cli-path-join
   (or (getenv "XDG_CONFIG_HOME") "~/.config") "org-export")
  "Directory for config files")

(defvar cli-config-file
  (cli-path-join cli-config-dir "config.el")
  "File for user config")

(defvar cli-packages '(htmlize color-theme-modern ess)
  "elisp packages installed by each script")

;; store the execution path for the current environment and provide it
;; to sh code blocks - otherwise, some system directories are
;; prepended in the code block's environment. Would be nice to figure
;; out where these are coming from. This solves the problem for shell
;; code blocks, but not for other languages (like python).
(defvar cli-sh-src-prologue
  (format "export PATH=\"%s\"" (mapconcat 'identity exec-path ":")))

(defun cli-do-nothing () t)

(defun cli-option-p (str)
  "Return t if str is not nil and starts with '--'"
  (and str (string-equal (substring str 0 2) "--")))

(defun cli-opt-name (str)
  "Return the option name (strips leading '--')"
  (substring str 2 nil))

(defun cli-required-p (optdef)
  "Return t if the option defined in `optdef` (an element of
`options-alist') is required."
  (eq (length optdef) 2))

(defun cli-parse-args (options-alist &optional docstring arguments)
  "Parses a list of arguments according to the specifiction
provided by `options-alist' and returns a hashmap of option names
to values. Arguments are read from `command-line-args' unless an
optional list `arguments' is provided.

`options-alist' is of the form

'((required-option help-text)
  (option help-text default-value)
  (boolean-option help_text nil))

Each option must be a string beginning with '--'. If
default-value is not provided, the option is required and an
error will be raised if it is missing.

Arguments are of the form '--option <value>'. If 'value' is
missing and a default value is defined, 'option' will be given a
value of t (this is useful for defining boolean command line
parameters).

Command line arguments already defined by emacs (see 'emacs
--help') are reserved and cannot be used.

Note that this function has a side effect: arbitrary command line
arguments are allowed by assigning `command-line-functions` a
value of `cli-do-nothing'.
"

  (setq command-line-functions '(cli-do-nothing))
  (let ((clargs (or arguments command-line-args))
	(args (make-hash-table :test 'equal))
	(opt nil)
	(optname nil)
	(optiondef nil)
	(val nil)
	(hashval nil))

    ;; print help text and exit if command line contains -h
    (if (member "-h" clargs)
	(progn (cli-show-help options-alist docstring)
	       (kill-emacs 0)))

    ;; set defaults
    (mapc #'(lambda (optdef)
	      (if (eq (length optdef) 3)
		  (puthash (cli-opt-name (car optdef)) (nth 2 optdef) args))
	      ) options-alist)

    ;; set options from command line arguments
    (while clargs
      (setq opt (car clargs))
      (setq optname (if (cli-option-p opt) (cli-opt-name opt)))
      (if optname
	  (progn
	    (setq optiondef (assoc opt options-alist))
	    (if optiondef
		;; If val is provided, add it to args. Otherwise,
		;; store a value of t unless the option is required.
		(progn
		  (unless (cli-option-p (nth 1 clargs))
                    (setq val (nth 1 clargs)))
		  (if (not (cli-required-p optiondef))
                      (setq val (or val t)))
		  (puthash optname val args))
	      (error (format "Error: the option '%s' is not defined" opt)))
	    ))
      (setq clargs (cdr clargs)))

    ;; check for required arguments
    (mapc #'(lambda (optdef)
	      (setq hashval (gethash (cli-opt-name (car optdef)) args))
	      (if (and (eq (length optdef) 2) (not hashval))
		  (error
                   (format
                    "Error: a value for the option '%s' is required"
                    (car optdef))))
	      ) options-alist)
    args
    ))

(defun cli-show-help (options-alist &optional docstring)
  "Display options, defaults and help text defined in
`options-alist' (see `cli-parse-args' for specification)"
  (let ((max-width
	 (apply 'max
                (mapcar #'(lambda (opt) (length (car opt))) options-alist)))
	(fstr nil))

    (princ "Command line options:\n\n")
    (setq fstr (format " %%-%ss  %%s\n" max-width))
    (mapc #'(lambda (opt)
	      (princ (format fstr (nth 0 opt) (nth 1 opt))))
	  options-alist)
    (princ "\n")
    (if docstring (princ docstring))
    (princ "\n")))

(defun cli-el-get-setup (emacs-directory package-list &optional upgrade archives)
  (unless (file-readable-p emacs-directory)
    (message (format "Creating directory %s" emacs-directory))
    (make-directory emacs-directory t))

  ;; must assign `user-emacs-directory' *before* requiring `package'
  (setq user-emacs-directory emacs-directory)

  (unless (file-readable-p cli-config-dir)
    (message (format "Creating directory %s" cli-config-dir))
    (make-directory cli-config-dir t))

  ;; install el-get if necessary
  (setq cli-el-get-repo
	(concat (file-name-as-directory user-emacs-directory) "el-get"))

  (unless (file-exists-p cli-el-get-repo)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (eval-region url-http-end-of-headers (point-max))))

  (add-to-list 'load-path (concat cli-el-get-repo "/el-get"))

  (require 'el-get)

  ;; Use el-get to install packages in 'package-list' using a while
  ;; loop and evaluating an expanded macro for each.
  (let ((pkg nil)
	(pkg-list package-list))
    (while pkg-list
      (setq pkg (car pkg-list))
      (setq pkg-list (cdr pkg-list))
      (eval (macroexpand `(el-get-bundle ,pkg)))))

  (if upgrade
      (progn
        (package-list-packages)
	(package-menu-mark-upgrades)
        (condition-case nil
            (package-menu-execute t)
          (error (message "No package updates available"))))
    (package-list-packages-no-fetch)))

;; other utilities

(defun cli-eval-expr (instr)
  "evaluate the input string in a temporary buffer for side effects"
  (if instr
      (with-temp-buffer
        (insert instr)
        (eval-buffer))))

(defun cli-eval-file (file-path)
  "evaluate the specified file in a temporary buffer for side effects"
  (if (and file-path (file-exists-p file-path))
      (with-temp-buffer
        (message "loading %s" file-path)
        (insert-file-contents file-path)
        (eval-buffer))))

(defun cli-replace-all (from-str to-str)
  "Replace all occurrences of from-str with to-str in current buffer"
  (beginning-of-buffer)
  (while (search-forward from-str nil t)
    (replace-match to-str nil t)))

(defun cli-get-output-file (fname otherfile suffix)
  "Return expanded, absolute path of `fname', or if `fname' is nil,
`otherfile' with its suffix replaced by `suffix'"
  (let ((outfile
         (or fname (concat (file-name-sans-extension otherfile) suffix))))
    (expand-file-name outfile)))

(defvar cli-org-babel-languages-default
  '("R" "dot" "emacs-lisp" "latex" "python" "shell" "sql" "sqlite")
  "Startng values for list of languages for org-babel code blocks")

(defun cli-org-babel-load-languages (extra-langs)
  "Load languages named in `cli-org-babel-languages-default' plus
any identified in comma-delimited string `extra-langs'"
  (let* ((extras (if extra-langs (split-string extra-langs ",")))
         (language-names
          (flatten-tree (cons cli-org-babel-languages-default extras))))

    (org-babel-do-load-languages
     'org-babel-load-languages
     (mapcar #'(lambda (lang) `(,(make-symbol lang) . t)) language-names))
    ))

;; only executed if this is the script called from the command line
;; (like python's "if __name__ == '__main__'")
(if (member (file-name-nondirectory load-file-name)
	    (mapcar 'file-name-nondirectory command-line-args))
    (progn
      (setq options-alist
	    `(("--package-dir" "directory containing elpa packages" ,cli-package-dir)
	      ("--package-upgrade" "Perform package upgrade" nil)
	      ("--show-package-dir" "Print the path to package-dir" nil)
              ("--show-default-languages" "list the languages that are activated by default" nil)
	      ))

      (defvar docstring "\nManage elpa packages\n")

      (setq args (cli-parse-args options-alist docstring))
      (defun getopt (name) (gethash name args))
      (cli-eval-file cli-config-file)

      (if (getopt "show-package-dir")
          (progn
            (print (getopt "package-dir"))
            (kill-emacs 0)))

      (if (getopt "show-default-languages")
          (progn
            (print cli-org-babel-languages-default)
            (kill-emacs 0)))

      (cli-el-get-setup
       (getopt "package-dir") cli-packages (getopt "package-upgrade"))

      ))
