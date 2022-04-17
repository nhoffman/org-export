(require 'cli (concat (file-name-directory load-file-name) "org-export-cli.el"))

;; (byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))
(setq options-alist
      `(("--infile" "path to input .org file")
	("--outfile" "path to output .html file (use base name of infile by default)"
	 nil)
        ("--add-langs" "comma-delimited list of additional languages to enable in code blocks" nil)
	("--package-dir" "directory containing elpa packages" ,cli-package-dir)))

(setq args (cli-parse-args options-alist))
(defun getopt (name) (gethash name args))
(cli-el-get-setup (getopt "package-dir") cli-packages)
(require 'ox)
(require 'ox-html)

;; ess configuration
(add-hook 'ess-mode-hook
	  #'(lambda ()
	      (setq ess-ask-for-ess-directory nil)))

;; org-mode and export configuration
(add-hook 'org-mode-hook
	  #'(lambda ()
	     ;; (font-lock-mode)
	     (setq org-src-fontify-natively nil)
	     (setq org-confirm-babel-evaluate nil)
	     (setq org-export-allow-BIND 1)
	     ;; (setq org-export-preserve-breaks t)
	     (setq org-export-with-sub-superscripts nil)
	     (setq org-html-table-default-attributes
	     	   '(:class "table table-striped table-bordered table-condensed"
			    :style "width: auto;"))
	     (setq org-export-with-section-numbers nil)
	     (setq org-babel-sh-command "bash")
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
	     ;; explicitly set the PATH in sh code blocks; note that
	     ;; `list`, the backtick, and the comma are required to
	     ;; dereference cli-sh-src-prologue as a variable; see
	     ;; http://stackoverflow.com/questions/24188100
	     (setq org-babel-default-header-args:sh
		   (list `(:prologue . ,cli-sh-src-prologue)))

	     ;; (setq org-export-htmlize-output-type 'css)
             (cli-org-babel-load-languages (getopt "add-langs"))

	     )) ;; end org-mode-hook

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; compile and export ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; evaluate extra configuration if provided
(cli-eval-file cli-config-file)

(defvar infile (getopt "infile"))
(defvar outfile
  (file-truename
   (or (getopt "outfile") (replace-regexp-in-string "\.org$" ".html" infile))))
(defvar save-as (file-name-nondirectory outfile))

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

(defun org-html-fontify-code (code lang)
  "Replaces the original function to suppress syntax
highlighting"
  (when code
    (org-html-encode-plain-text code)))

(defun plist-get-as-text (plist attr)
  "Get attributes from the output of org-export-get-environment"
  (let ((str (car (plist-get plist attr))))
    (if str (substring-no-properties str) nil)))

;; these values need to be defined here, ie, after we activate
;; org-mode and before we call org-html-export-as-html
;; see https://github.com/ardekantur/pelican-plugins/tree/org_reader/org_reader
(defvar properties (org-export-get-environment))
(defvar html-head
  (let ((title (plist-get-as-text properties :title))
	(author (plist-get-as-text properties :author))
	(date (plist-get-as-text properties :date))
        ;; read category from CATEGORY file header
        (category (cdr (assoc "CATEGORY" (org-entry-properties))))
        ;; read tags from FILETAGS file header; expect comma delimited string like
        ;; #+FILETAGS: mac,notes
	(tags (car org-file-tags)))
    (format "
<html>
    <head>
        <title>%s</title>
        <meta name=\"authors\" content=\"%s\" />
        <meta name=\"date\" content=\"%s\" />
        <meta name=\"category\" content=\"%s\" />
        <meta name=\"tags\" content=\"%s\" />
        <meta name=\"save_as\" content=\"%s\" />
        <meta name=\"url\" content=\"%s\" />
    </head>
    <body>" title author date category tags save-as save-as)))

;; might want to add these fields later
;; <meta name=\"modified\" content=\"2012-07-10 20:14\" />
;; <meta name=\"summary\" content=\"Short version for index and feeds\" />

(org-html-export-as-html nil nil nil t)

;; Insert html header and footer with metadata for pelican
(insert html-head)
(goto-char (point-max))
(insert "</body></html>")

;; all done
(write-file outfile)

;; clean up
(setq default-directory cwd)
(delete-file infile-temp)

