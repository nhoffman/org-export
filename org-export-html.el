(require 'cli (concat (file-name-directory load-file-name) "org-export-cli.el"))

;; (byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))
(setq options-alist
      `(("--infile" "path to input .org file (required)")
	    ("--outfile" "path to output .html file (use base name of infile by default)" nil)
        ("--add-langs" "comma-delimited list of additional languages to enable in code blocks" nil)
	    ("--evaluate" "evaluate source code blocks" nil)
	    ("--css" "path or URL of css stylesheet" nil)
	    ("--embed-css" "Include contents of css in a <style> block" nil)
	    ("--bootstrap" "make Bootstrap-specific modifications to html output;
                        if selected, link to Bootstrap CDN by default" nil)
	    ("--package-dir" "directory containing elpa packages" ,cli-package-dir)
        ("--config" "an elisp expression defining additional configuration" nil)
        ("--config-file" "a file path containing elisp expressions defining additional configuration" nil)
	    ("--verbose" "enable debugging message on error" nil)
	    ))

(setq args (cli-parse-args options-alist "
Note that code block evaluation is disabled by default; use
'--evaluate' to set a default value of ':eval yes' for all code
blocks. If you would like to evaluate by default without requiring
this option, include '#+PROPERTY: header-args :eval yes' in the file
header. Individual blocks can be selectively evaluated using ':eval
yes' in the block header.
"))
(defun getopt (name) (gethash name args))
(cli-el-get-setup (getopt "package-dir") cli-packages)

(require 'ox)
(require 'ox-html)

;; provides colored syntax highlighting
(condition-case nil
    (require 'color-theme-modern)
  (error (message "** could not activate color-theme-modern")))

(setq debug-on-error (getopt "verbose"))
;; (setq debug-on-signal (getopt "debug"))

;; general configuration
(setq make-backup-files nil)

;; ess configuration
(add-hook 'ess-mode-hook
	  #'(lambda ()
	     (setq ess-ask-for-ess-directory nil)))

;; css configuration
(defvar bootstrap-url
  "http://netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css")

(defvar css-url (getopt "css"))
(if (getopt "bootstrap")
    (setq css-url (or css-url bootstrap-url)))

(defvar my-html-head "")
(if css-url
    (if (getopt "embed-css")
	;; embed css contents in a <style> block
	(progn
	  (setq my-html-head
		(format "<style type=\"text/css\">\n%s\n</style>\n"
			(if (string-match "^http" css-url)
			    ;; use the contents of file at path
			    (with-current-buffer
				(url-retrieve-synchronously css-url)
			      (message (format "Inserting contents of %s" css-url))
			      (buffer-string))
			  ;; use the contents of the file at css-url
			  (with-temp-buffer
			    (insert-file-contents css-url)
			    (buffer-string)))
			)))
      ;; ...or add a link to the css file
      (setq my-html-head
	    (format
	     "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />" css-url))))

;; org-mode and export configuration

;; store the execution path for the current environment and provide it
;; to sh code blocks - otherwise, some system directories are
;; prepended in the code block's environment. Would be nice to figure
;; out where these are coming from. This solves the problem for shell
;; code blocks, but not for other languages (like python).
(defvar exec-path-str
  (mapconcat 'identity exec-path ":"))
(defvar sh-src-prologue
  (format "export PATH=\"%s\"" exec-path-str))

(add-hook 'org-mode-hook
	  #'(lambda ()
	     ;; (font-lock-mode)
	     ;; (setq org-src-fontify-natively t)
	     ;; (setq htmlize-output-type 'inline-css)
	     (setq org-confirm-babel-evaluate nil)
	     (setq org-export-allow-BIND 1)
	     ;; (setq org-export-preserve-breaks t)
	     ;; (setq org-export-with-sub-superscripts nil)
	     ;; (setq org-export-with-section-numbers nil)
	     (setq org-html-doctype "html5")
	     (setq org-html-head my-html-head)
	     ;; (setq org-html-head-extra my-html-head-extra)
	     (setq org-babel-sh-command "bash")
	     (setq org-babel-default-header-args
		   (list `(:session . "none")
			 `(:eval . ,(if (getopt "evaluate") "yes" "no"))
			 `(:results . "output replace")
			 `(:exports . "both")
			 `(:cache . "no")
			 `(:noweb . "no")
			 `(:hlines . "no")
			 `(:tangle . "no")
			 `(:padnewline . "yes")
			 ))

	     ;; explicitly set the PATH in sh code blocks; note that
	     ;; `list`, the backtick, and the comma are required to
	     ;; dereference sh-src-prologue as a variable; see
	     ;; http://stackoverflow.com/questions/24188100
	     (setq org-babel-default-header-args:sh
		   (list `(:prologue . ,sh-src-prologue)))

         (cli-org-babel-load-languages (getopt "add-langs"))

	     )) ;; end org-mode-hook

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; compile and export ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evaluate extra configuration if provided
(cli-eval-expr (getopt "config"))
(cli-eval-file (getopt "config-file"))

(defvar infile (getopt "infile"))
(defvar outfile
  (file-truename
   (or (getopt "outfile") (replace-regexp-in-string "\.org$" ".html" infile))))

;; remember the current directory; find-file changes it
(defvar cwd default-directory)
;; copy the source file to a temporary file; note that using the
;; infile as the base name defines the working directory as the same
;; as the input file
(defvar infile-temp (make-temp-name (format "%s.temp." infile)))
(copy-file infile infile-temp t)
(find-file infile-temp)
(org-mode)
(message (format "org-mode version %s" org-version))
(org-html-export-as-html)

;; It is not possible to add attributes to certain elements (eg,
;; <body>) using org-mode configuration, so we'll just use string
;; replacement as necessary.
(if (getopt "bootstrap")
    (progn
      (cli-replace-all "<body>" "<body class=\"container\">")
      (cli-replace-all
       "<table>"
       "<table class=\"table table-striped table-bordered table-condensed\"
         style=\"width: auto;\">")
      (cli-replace-all "<dl class=\"org-dl\">" "<dl class=\"dl-horizontal\">")))

(write-file outfile)

;; clean up
(setq default-directory cwd)
(delete-file infile-temp)
