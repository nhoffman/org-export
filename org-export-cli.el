(unless (>= emacs-major-version 24)
  (error "Error: emacs version 24 or greater is required"))

;; (setq debug-on-error t)

(setq lexical-binding t)
(provide 'cli)

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

    ;; print help text and exit if command line contains -h or -h-org
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
		  (unless (cli-option-p (nth 1 clargs)) (setq val (nth 1 clargs)))
		  (if (not (cli-required-p optiondef)) (setq val (or val t)))
		  (puthash optname val args))
	      (error (format "Error: the option '%s' is not defined" opt)))
	    ))
      (setq clargs (cdr clargs)))

    ;; check for required arguments
    (mapc #'(lambda (optdef)
	      (setq hashval (gethash (cli-opt-name (car optdef)) args))
	      (if (and (eq (length optdef) 2) (not hashval))
		  (error (format "Error: a value for the option '%s' is required"
				 (car optdef))))
	      ) options-alist)

    args
    ))

(defun cli-show-help (options-alist &optional docstring)
  "Display options, defaults and help text defined in
`options-alist' (see `cli-parse-args' for specification)"
  (let ((max-width
	 (apply 'max (mapcar #'(lambda (opt) (length (car opt))) options-alist)))
	(fstr nil))

    (princ "Command line options:\n\n")
    (setq fstr (format " %%-%ss  %%s\n" max-width))
    (mapc #'(lambda (opt)
	      (princ (format fstr (nth 0 opt) (nth 1 opt))))
	  options-alist)
    (princ "\n")
    (if docstring (princ docstring))
    (princ "\n")))

;; package management
;; (defun cli-package-installed-p (package &optional min-version)
;;   "Return true if PACKAGE, of MIN-VERSION or newer, is installed
;; (ignoring built-in versions).  MIN-VERSION should be a version list"

;;   (unless package--initialized (error "package.el is not yet initialized!"))

;; (if (and (< emacs-major-version 24) (< emacs-minor-version 4))
;;     ;; < emacs 24.4
;;     (let ((pkg-desc (assq package package-alist)))
;;       (if pkg-desc
;; 	  (version-list-<= min-version
;; 			   (package-desc-vers (cdr pkg-desc)))))
;;   ;; >= emacs 24.4
;;   (let ((pkg-descs (cdr (assq package package-alist))))
;;     (and pkg-descs
;; 	 (version-list-<= min-version
;; 			  (package-desc-version (car pkg-descs)))))
;;   ))

;; (defun cli-install-packages (package-list)
;;   "Install each package named in PACKAGE-LIST using elpa if not
;; already installed."
;;   (mapc #'(lambda (pkg)
;; 	    (unless (cli-package-installed-p pkg)
;; 	      (message "installing %s..." pkg)
;; 	      (unless package-archive-contents
;; 		(package-refresh-contents))
;; 	      ;; (package-menu-refresh)
;; 	      (package-install pkg)
;; 	      (message "done installing %s" pkg))
;; 	    ) package-list))

;; (defvar cli-default-package-archives
;;   '(("gnu" . "http://elpa.gnu.org/packages/")
;;     ("marmalade" . "http://marmalade-repo.org/packages/")
;;     ("melpa" . "http://melpa.org/packages/")
;;     ("org" . "http://orgmode.org/elpa/")))

;; (defun cli-package-setup (emacs-directory package-list &optional upgrade archives)
;;   (unless (file-readable-p emacs-directory)
;;     (message (format "Creating directory %s" emacs-directory))
;;     (make-directory emacs-directory t))

;;   ;; must assign `user-emacs-directory' *before* requiring `package'
;;   (setq user-emacs-directory emacs-directory)

;;   ;; also seems to be necessary to define `package-archives' before
;;   ;; requiring `package'
;;   (setq package-archives (or archives cli-default-package-archives))

;;   (require 'package)
;;   (package-initialize)

;;   (if upgrade
;;       (progn
;; 	(package-list-packages)
;;   	(package-menu-mark-upgrades)
;;   	(package-menu-execute))
;;     (package-list-packages-no-fetch))

;;   (cli-install-packages package-list))

(defun cli-el-get-setup (emacs-directory package-list &optional upgrade archives)
  (unless (file-readable-p emacs-directory)
    (message (format "Creating directory %s" emacs-directory))
    (make-directory emacs-directory t))

  ;; must assign `user-emacs-directory' *before* requiring `package'
  (setq user-emacs-directory emacs-directory)

  ;; fix TLS certificate errors
  ;; http://emacs.stackexchange.com/questions/18045/how-can-i-retrieve-an-https-url-on-mac-os-x-without-warnings-about-an-untrusted
  (if (and (eq system-type 'darwin)
	   (file-exists-p "/usr/local/etc/libressl/cert.pem"))
      (progn
	(require 'gnutls)
	(setq gnutls-verify-error t)
	(add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem")))

  ;; install el-get if necessary
  (setq cli-el-get-repo
  	(concat (file-name-as-directory user-emacs-directory) "el-get"))
  (unless (file-exists-p cli-el-get-repo)
    (with-current-buffer
  	(url-retrieve-synchronously
  	 "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
      (eval-region url-http-end-of-headers (point-max))
      ))
  (add-to-list 'load-path (concat cli-el-get-repo "/el-get"))
  ;; (add-to-list 'load-path cli-el-get-repo)

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
  	(package-menu-execute))
    (package-list-packages-no-fetch))
  )

;; other utilities

(defun cli-replace-all (from-str to-str)
  "Replace all occurrences of from-str with to-str in current buffer"
  (beginning-of-buffer)
  (while (search-forward from-str nil t)
    (replace-match to-str nil t)))

(defun cli-get-org-babel-load-languages ()
  "enable a subset of languages for evaluation in code blocks"
  (let ((lang-list '((R . t)
		    (latex . t)
		    (python . t)
		    (sql . t)
		    (sqlite . t)
		    (emacs-lisp . t)
		    (dot . t))))

    ;; use "shell" for org-mode versions 9 and above
    (add-to-list
     'lang-list
     (if (>= (string-to-number (substring (org-version) 0 1)) 9)
	 '(shell . t) '(sh . t)))

    lang-list))

;; only executed if this is the script called from the command line
;; (like python's "if __name__ == '__main__'")
(if (member (file-name-nondirectory load-file-name)
	    (mapcar 'file-name-nondirectory command-line-args))
    (progn
      (setq options-alist
	    '(("--package-dir" "directory containing elpa packages" "~/.org-export")
	      ("--package-upgrade" "Perform package upgrade" nil)
	      ))

      (defvar docstring "\nManage elpa packages\n")

      (setq args (cli-parse-args options-alist docstring))
      (defun getopt (name) (gethash name args))

      (cli-el-get-setup
       (getopt "package-dir") '(org htmlize) (getopt "package-upgrade"))
      ))
