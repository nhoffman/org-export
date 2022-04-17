(require 'cli (concat (file-name-directory load-file-name) "org-export-cli.el"))

;; (byte-compile-file (concat (file-name-directory load-file-name) "cli.el"))
(setq options-alist
      `(("--infile" "path to input .org file")
        ("--add-langs" "comma-delimited list of additional languages to enable in code blocks" nil)
	("--package-dir" "directory containing elpa packages" ,cli-package-dir)
	))

(setq args (cli-parse-args options-alist))

(defun getopt (name) (gethash name args))
(cli-el-get-setup (getopt "package-dir") cli-packages)

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
	      ;; dereference cli-sh-src-prologue as a variable; see
	      ;; http://stackoverflow.com/questions/24188100
	      (setq org-babel-default-header-args:sh
		    (list `(:prologue . ,cli-sh-src-prologue)))

              (cli-org-babel-load-languages (getopt "add-langs"))

	      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; compile and export ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((infile (expand-file-name (getopt "infile")))
       (infile-temp (make-temp-name (format "%s.temp." infile))))
  (copy-file infile infile-temp t)
  (find-file infile-temp)
  (org-mode)
  (condition-case t
      (org-babel-tangle))
  (delete-file infile-temp))
