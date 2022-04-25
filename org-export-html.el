(require 'cli (concat (file-name-directory load-file-name) "org-export-cli.el"))
(cli-el-get-setup cli-package-dir cli-packages)
(require 'request)
(require 'ox)
(require 'ox-html)

(defvar oe-html-css-styles
  '((:name "bootstrap5"
           :url "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css"
           :integrity "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC"
           :reference "https://getbootstrap.com/docs/5.1/getting-started/introduction/"
           :pre-export (lambda ()
                         (setq org-html-htmlize-output-type 'css))
           :post-export (lambda () (oe-html-fix-bootstrap)))
    (:name "orgcss"
           :url "https://gongzhitaao.org/orgcss/org.css"
           :reference "https://github.com/gongzhitaao/orgcss"
           :pre-export (lambda ()
                         (setq org-html-htmlize-output-type 'css)
                         (setq org-html-head-include-default-style nil)))
    (:name "org-manual"
           :url "https://www.gnu.org/software/emacs/manual.css"
           :reference "https://orgmode.org"
           :pre-export (lambda ()
                         (setq org-html-htmlize-output-type 'css)
                         (setq org-html-head-include-default-style nil)))
    ))

(defvar oe-html-css-style-names
  (mapconcat (lambda (plist) (format "'%s'" (plist-get plist ':name))) oe-html-css-styles ", "))

(defvar oe-html-pre-export nil)
(defvar oe-html-post-export nil)

(defun oe-html-fix-bootstrap ()
  "Make adjustments to html in current buffer for bootstrap"
  (cli-replace-all "<body>" "<body class=\"container\">")
  (cli-replace-all
   "<table>"
   "<table class=\"table table-bordered table-sm\" style=\"width: auto;\">")
  )

(defun oe-html-css-link (url &optional integrity)
  "Return a <link> tag specifying a css style"
  (if integrity
      (format
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\"
                integrity=\"%s\" crossorigin=\"anonymous\" />"
       url integrity)
    (format
     "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />"
     url)))

(defun oe-html-css-content (url)
  "Return a <style> tag with embedded css"
  (message (format "Inserting contents of %s" url))
  (format "<style type=\"text/css\">\n%s\n</style>\n"
          (if (string-match "^http" url)
              (request-response-data
               (request url :parser 'buffer-string :sync t))
            (with-temp-buffer
              (insert-file-contents url)
              (buffer-string)))))

(defun oe-get-plist-by-name (name list-of-plists)
  (car (remq nil (mapcar
                  (lambda(plist)
                    (if (string-equal name (plist-get plist ':name))
                        plist))
                  list-of-plists))))

;; (byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))
(setq options-alist
      `(("--infile" "Path to input .org file (required)")
	("--outfile"
         "Path to output .html file (use base name of infile by default)"
         nil)
        ("--add-langs"
         "Comma-delimited list of additional languages
          to enable in code blocks"
         nil)
	("--evaluate" "Evaluate source code blocks" nil)
	("--css-name"
         ,(format "Use the named css style; choose from %s" oe-html-css-style-names)
         nil)
	("--css" "Path or URL of css stylesheet" nil)
        ("--css-integrity"
         "Optional value for css link integrity attribute" nil)
	("--embed-css" "Include contents of css in a <style> block" nil)
	("--package-dir"
         "Directory containing elpa packages" ,cli-package-dir)
        ("--config"
         "An elisp expression defining additional configuration" nil)
        ("--config-file"
         "A file path providing  additional configuration" nil)
	))

(setq docstring "
Note that code block evaluation is disabled by default; use
'--evaluate' to set a default value of ':eval yes' for all code
blocks. If you would like to evaluate by default without requiring
this option, include '#+PROPERTY: header-args :eval yes' in the file
header. Individual blocks can be selectively evaluated using ':eval
yes' in the block header.")

(condition-case err
    (setq args (cli-parse-args options-alist docstring))
  (quit (kill-emacs 0))
  (error (progn (message (nth 1 err)) (kill-emacs 1))))

(defun getopt (name) (gethash name args))
(cli-el-get-setup (getopt "package-dir") cli-packages)

;; css configuration
(cond
 ((getopt "css")
  (setq css-config
        `(:name "custom" :url ,(getopt "css") :integrity ,(getopt "css-integrity"))))
 ((getopt "css-name")
  (setq css-config
        (oe-get-plist-by-name (getopt "css-name") oe-html-css-styles))
  (unless css-config
    (message "'--css-name %s' is not a valid selection, choose from %s"
             (getopt "css-name") oe-html-css-style-names)
    (kill-emacs 1))
  (setq oe-html-pre-export (plist-get css-config ':pre-export))
  (setq oe-html-post-export (plist-get css-config ':post-export)))
 (t
  (setq css-config nil)))

(defvar oe-html-head
  (if css-config
      (let ((url (plist-get css-config ':url))
            (integrity (plist-get css-config ':integrity)))
        (if (getopt "embed-css")
            (oe-html-css-content url)
          (oe-html-css-link url integrity)))))

;; org-mode and export configuration
(add-hook 'org-mode-hook
	  (lambda ()
	    ;; (setq org-src-fontify-natively t)
	    ;; (setq htmlize-output-type 'inline-css)
	    (setq org-confirm-babel-evaluate nil)
	    (setq org-export-allow-BIND 1)
	    ;; (setq org-export-preserve-breaks t)
	    ;; (setq org-export-with-sub-superscripts nil)
	    ;; (setq org-export-with-section-numbers nil)
	    (setq org-html-doctype "html5")
	    (setq org-html-head oe-html-head)
	    ;; (setq org-html-head-extra my-html-head-extra)
	    (setq org-babel-sh-command "bash")
	    (setq org-babel-python-command "python3")
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

    ;; Required for syntax hightlighting
    (outline-show-all)
    (htmlize-buffer)
    (font-lock-flush)
    (font-lock-fontify-buffer)

    (if oe-html-pre-export
        (funcall oe-html-pre-export))

    (org-html-export-as-html)

    (if oe-html-post-export
        (funcall oe-html-post-export))

    (write-file outfile)
    (message "wrote %s" outfile)))
