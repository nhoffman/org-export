(require 'cli (concat (file-name-directory load-file-name) "org-export-cli.el"))

;; (byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))
(setq options-alist
      `(("--infile" "path to input .org file")
	("--package-dir" "directory containing elpa packages" ,cli-package-dir)
	("--verbose" "enable debugging message on error" nil)
	))

(setq args (cli-parse-args options-alist "
Option --infile is required.
"))
(defun getopt (name) (gethash name args))
(cli-el-get-setup
 (getopt "package-dir") '(ess org))

(setq debug-on-error (getopt "verbose"))
;; (setq debug-on-signal (getopt "debug"))

;; general configuration
(setq make-backup-files nil)

;; ess configuration
(add-hook 'ess-mode-hook
	  '(lambda ()
	     (setq ess-ask-for-ess-directory nil)
	     ))

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
	  '(lambda ()
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
			 `(:eval . "no")
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

	     (org-babel-do-load-languages
	      (quote org-babel-load-languages)
	      (quote ((R . t)
		      (latex . t)
		      (python . t)
		      (sh . t)
		      (sql . t)
		      (sqlite . t)
		      (emacs-lisp . t)
		      (dot . t)
		      )))
	     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; compile and export ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar infile (getopt "infile"))

;; remember the current directory; find-file changes it
(defvar cwd default-directory)
;; copy the source file to a temporary file; note that using the
;; infile as the base name defines the working directory as the same
;; as the input file
(defvar infile-temp (make-temp-name (format "%s.temp." infile)))
(copy-file infile infile-temp t)
(find-file infile-temp)
(org-mode)
(org-babel-tangle)

;; clean up
(setq default-directory cwd)
(delete-file infile-temp)
