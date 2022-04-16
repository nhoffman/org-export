(require 'cli (concat (file-name-directory load-file-name) "org-export-cli.el"))

;; (byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))
(setq options-alist
      `(("--infile" "path to input .org file (required)")
	("--outfile" "path to output .pdf file (use base name of infile by default)"
	 nil)
	("--evaluate" "evaluate source code blocks" nil)
	("--package-dir" "directory containing elpa packages" ,cli-package-dir)
        ("--config" "an elisp expression defining additional configuration" nil)
        ("--config-file" "a file path containing elisp expressions defining additional configuration" nil)
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
(require 'ox-latex)

;; provides colored syntax highlighting
(condition-case nil
    (require 'color-theme-modern)
  (error (message "** could not activate color-theme-modern")))

;; ess configuration
;; (add-hook 'ess-mode-hook
;; 	  '(lambda ()
;; 	     (setq ess-ask-for-ess-directory nil)))


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

(defvar infile (getopt "infile"))
(defvar outfile
  (file-truename
   (or (getopt "outfile") (replace-regexp-in-string "\.org$" ".pdf" infile))))

;; remember the current directory; find-file changes it
(defvar cwd default-directory)
;; copy the source file to a temporary file; note that using the
;; infile as the base name defines the working directory as the same
;; as the input file
;(defvar infile-temp (make-temp-name (format "%s.temp." infile)))
;(copy-file infile infile-temp t)
(find-file infile)
(org-mode)
(message (format "org-mode version %s" org-version))
(org-latex-export-to-pdf)
;(write-file outfile)

;; clean up
(setq default-directory cwd)
;(delete-file infile-temp)
