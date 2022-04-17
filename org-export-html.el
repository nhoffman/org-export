(require 'cli (concat (file-name-directory load-file-name) "org-export-cli.el"))

;; (byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))
(setq options-alist
      `(("--infile" "path to input .org file (required)")
	    ("--outfile"
         "path to output .html file (use base name of infile by default)"
         nil)
        ("--add-langs"
         "comma-delimited list of additional languages
          to enable in code blocks"
         nil)
	    ("--evaluate" "evaluate source code blocks" nil)
	    ("--css" "path or URL of css stylesheet" nil)
        ("--css-integrity"
         "optional value for css link integrity attribute" nil)
	    ("--embed-css" "Include contents of css in a <style> block" nil)
	    ("--bootstrap"
         "make Bootstrap-specific modifications to html output;
          if selected, link to Bootstrap CDN by default"
         nil)
	    ("--package-dir"
         "directory containing elpa packages" ,cli-package-dir)
        ("--config"
         "an elisp expression defining additional configuration" nil)
        ("--config-file"
         "a file path providing  additional configuration" nil)
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

;; css configuration
(defvar use-bootstrap
  (if (getopt "bootstrap") t nil))

(defvar bootstrap-url
  "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css")

(defvar bootstrap-integrity
  "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC")

(defvar css-url (getopt "css"))
(defvar css-intergrity (getopt "css-integrity"))

(if use-bootstrap
    (progn
      (setq css-url bootstrap-url)
      (setq css-integrity bootstrap-integrity)))

(defun html-fix-bootstrap ()
  "Make adjustments to html in current buffer for bootstrap"
  (cli-replace-all "<body>" "<body class=\"container\">")
  (cli-replace-all
   "<table>"
   "<table class=\"table table-bordered table-sm\" style=\"width: auto;\">")
  )

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
			                  (message
                               (format "Inserting contents of %s" css-url))
			                  (buffer-string))
			              ;; use the contents of the file at css-url
			              (with-temp-buffer
			                (insert-file-contents css-url)
			                (buffer-string)))
                        )))
      ;; ...or add a link to the css file
      (setq my-html-head
            (if css-integrity
	            (format
                 "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\"
                    integrity=\"%s\" crossorigin=\"anonymous\" />"
                 css-url css-integrity)
              (format
	           "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />"
               css-url)))
      ))

;; ess configuration
(add-hook 'ess-mode-hook
	  #'(lambda ()
	     (setq ess-ask-for-ess-directory nil)))

;; org-mode and export configuration
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
	     ;; dereference cli-sh-src-prologue as a variable; see
	     ;; http://stackoverflow.com/questions/24188100
	     (setq org-babel-default-header-args:sh
		   (list `(:prologue . ,cli-sh-src-prologue)))

         (cli-org-babel-load-languages (getopt "add-langs"))

	     )) ;; end org-mode-hook

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; compile and export ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evaluate extra configuration if provided
(cli-eval-file cli-config-file)
(cli-eval-expr (getopt "config"))
(cli-eval-file (getopt "config-file"))

;; export using a temporary buffer to avoid modifying input file; working
;; directory contains the input file.
(let* ((infile (expand-file-name (getopt "infile")))
       (outfile (cli-get-output-file (getopt "outfile") infile ".html")))
  (with-temp-buffer
    (insert-file-contents-literally infile)
    (cd (file-name-directory infile))
    (org-mode)
    (org-html-export-as-html)
    ;; It is not possible to add attributes to certain elements (eg,
    ;; <body>) using org-mode configuration, so we'll just use string
    ;; replacement as necessary.
    (if use-bootstrap (html-fix-bootstrap))
    (write-file outfile)
    (message "wrote %s" outfile)))
