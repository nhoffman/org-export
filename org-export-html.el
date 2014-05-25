(require 'cli (concat (file-name-directory load-file-name) "org-export-cli.el"))

;; (byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))
(setq options-alist
      '(("--infile" "path to input .org file")
	("--outfile" "path to output .html file (use base name of infile by default)"
	 nil)
	("--css" "path or URL of css" nil)
	("--embed-css" "Include contents of css in a <style> block" nil)
	("--bootstrap" "make Bootstrap-specific modifications to html output;
                        if selected, link to Bootstrap CDN by default" nil)
	("--package-dir" "directory containing elpa packages" "~/.org-export")
	))

(setq args (cli-parse-args options-alist))
(defun getopt (name) (gethash name args))
(cli-package-setup (getopt "package-dir") '(ess htmlize org))

;; general configuration
(setq make-backup-files nil)

;; ess configuration
(add-hook 'ess-mode-hook
	  '(lambda ()
	     (setq ess-ask-for-ess-directory nil)
	     ))

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
(add-hook 'org-mode-hook
	  '(lambda ()
	     (font-lock-mode)
	     (setq org-src-fontify-natively t)
	     ;; (setq org-pygment-path "/usr/local/bin/pygmentize")
	     (setq org-confirm-babel-evaluate nil)
	     (setq org-export-allow-BIND 1)
	     ;; (setq org-export-preserve-breaks t)
	     ;; (setq org-export-with-sub-superscripts nil)
	     ;; (setq org-export-with-section-numbers nil)
	     (setq org-html-doctype "html5")
	     (setq org-html-head my-html-head)
	     ;; (setq org-html-head-extra my-html-head-extra)
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
	     (org-babel-do-load-languages
	      (quote org-babel-load-languages)
	      (quote ((R . t)
		      (latex . t)
		      (python . t)
		      (sh . t)
		      (sql . t)
		      (sqlite . t)
		      (emacs-lisp . t)
		      ;; (pygment . t)
		      )))
	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; compile and export ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar infile (getopt "infile"))
(defvar outfile
  (file-truename
   (or (getopt "outfile") (replace-regexp-in-string "\.org$" ".html" infile))))

;; remember the current directory; find-file changes it
(defvar cwd default-directory)
(defvar infile-temp (make-temp-name outfile))
(copy-file infile infile-temp t)
(find-file infile-temp)
(org-mode)
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
