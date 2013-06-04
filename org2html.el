#!emacs --script

;; Export an org-mode file to html
;;
;; Usage: org2html.el -infile in.org -outfile out.html [-package-dir path/to/elpa-package-dir]

;; functions for processing command line arguments
;; http://ergoemacs.org/emacs/elisp_hash_table.html
(defun is-option (str)
  ;; return true if string looks like a command line option
  (string-equal (substring str 0 1) "-"))

(defun get-option (args opt &optional default)
  ;; Return the value of "opt" from "args"; if there is no value for
  ;; "opt" return "default" if provided, otherwise raise an error.
  (or (or (gethash opt args) default)
      (error (format "Error: option -%s is required" opt))))

(defun replace-all (from-str to-str)
  ;; replace all occurrences of from-str with to-str
  (progn
    (beginning-of-buffer)
    (while (search-forward from-str nil t)
      (replace-match to-str nil t))))

;; allows arbitrary command line arguments
(defun do-nothing () t)
(setq command-line-functions '(do-nothing))

;; store option, value pairs in hash-map `args`
(defvar args (make-hash-table :test 'equal))

;; process command-line-args
(setq clargs command-line-args)
(while clargs
  (setq opt (car clargs))
  (setq val (car (cdr clargs)))
  (if (and (is-option opt) (not (is-option val)))
      (puthash (substring opt 1 nil) val args))
  (setq clargs (cdr clargs)))

;; get command line options
(setq infile (get-option args "infile"))
(setq outfile (get-option args "outfile"))

;; -package-dir defines where elpa should find or install packages and
;; package data
(setq user-emacs-directory (get-option args "package-dir" "~/.org-export"))

(message (format "using packages in %s" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; install and initialize packages as necessary
(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(defun package-installed-not-builtin-p (package &optional min-version)
  "Return true if PACKAGE, of MIN-VERSION or newer, is installed, ignoring built in packages.
MIN-VERSION should be a version list."
  (let ((pkg-desc (assq package package-alist)))
    (if pkg-desc
        (version-list-<= min-version
                         (package-desc-vers (cdr pkg-desc))))))

;; required packages here
(defvar package-my-package-list '(ess htmlize org))

(defun package-install-list (package-list)
  ;; Install each package named in "package-list" using elpa if not
  ;; already installed.
  (while package-list
    (setq pkg (car package-list))
    (unless (package-installed-not-builtin-p pkg)
      (package-menu-refresh)
      (package-install pkg))
    (setq package-list (cdr package-list)))
  ;; (message "done installing packages")
)

(defun package-install-my-packages ()
  ;; Interactively installs packages listed in global 'package-my-package-list'
  (interactive)
  (package-list-packages-no-fetch)
  (package-install-list package-my-package-list))

(package-install-my-packages)

;; org-babel-remove-result
;; org-babel-next-src-block

(defun org-babel-remove-all-results ()
  (interactive)
  (while (org-babel-next-src-block)
    (org-babel-remove-result)))

;; org-mode configuration

;; bootstrap-specific configuration
(setq my-html-head
      "<link rel=\"stylesheet\" type=\"text/css\"
       href=\"http://twitter.github.io/bootstrap/assets/css/bootstrap.css\" />")
;; overrides bootstrap default value of "width: 100%"
(setq my-html-head-extra
      "<style type=\"text/css\">.table {width: auto;}</style>")
(setq my-html-doctype "<!DOCTYPE html>")
(setq my-html-table-default-attributes
      '(:class "table table-striped table-condensed table-bordered table-hover"))

(add-hook 'org-mode-hook
	  '(lambda ()
	     (turn-on-font-lock)
	     (setq org-src-fontify-natively t)
	     (setq org-pygment-path "/usr/local/bin/pygmentize")
	     (setq org-confirm-babel-evaluate nil)
	     (setq org-export-allow-BIND 1)
	     (setq org-export-html-coding-system 'utf-8)
	     (setq org-export-html-postamble nil)
	     (setq org-export-preserve-breaks t)
	     (setq org-export-with-sub-superscripts nil)
	     (setq org-export-with-section-numbers nil)
	     (setq org-html-doctype my-html-doctype)
	     (setq org-html-head my-html-head)
	     (setq org-html-head-extra my-html-head-extra)
	     (setq org-html-table-default-attributes
		   my-html-table-default-attributes)
	     (setq org-babel-default-header-args
		   '((:session . "none")
		     (:results . "output replace")
		     (:exports . "both")
		     (:cache . "no")
		     (:noweb . "no")
		     (:hlines . "no")
		     (:tangle . "no")
		     (:padnewline . "yes")
		     ))
	     ;; (setq org-export-htmlize-output-type 'css)
	     ;; (setq org-export-with-toc nil)
	     (org-babel-do-load-languages
	      (quote org-babel-load-languages)
	      (quote ((R . t)
		      (latex . t)
		      (python . t)
		      (sh . t)
		      (sql . t)
		      (sqlite . t)
		      ;; (pygment . t)
		      (emacs-lisp . t)
		      )))
	     ))

;; general configuration
(setq make-backup-files nil)

;; ess configuration
(add-hook 'ess-mode-hook
	  '(lambda ()
	     (setq ess-ask-for-ess-directory nil)
	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; compile and export ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; save the current directory; find-file seems to change it
(setq cwd default-directory)

;; copy file containing the post to a tempfile
(setq infile-temp (make-temp-name "org2html-"))
(copy-file infile infile-temp t)
(find-file infile-temp)
(org-mode)

(org-html-export-as-html)
;; until I figure out how to assign specific elements to a class using
;; org-mode configuration...

;; required for bootstrap
(while (search-forward "<body>" nil t)
  (replace-match "<body class=\"container\">"))

(write-file outfile)

;; clean up
(setq default-directory cwd)
(delete-file infile-temp)
