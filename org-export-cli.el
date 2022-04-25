(setq lexical-binding t)
(unless (>= emacs-major-version 26)
  (error "Error: emacs version 26 or greater is required"))

(provide 'cli)
(require 'cl-lib)

;; configuration applying to all commands
(setq make-backup-files nil)
(setq debug-on-error t)

(setq python-shell-completion-native-enable nil)
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)

(defun cli-path-join (&rest parts)
  "Join elements of list `parts' with a path separator and apply
`expand-file-name'"
  (expand-file-name
   (mapconcat 'identity
              (append (mapcar 'file-name-as-directory (butlast parts)) (last parts))
              "")))

(defvar cli-package-dir
  (cli-path-join
   (or (getenv "XDG_DATA_HOME") "~/.local/share") "org-export")
  "Location to install org-mode and dependencies")

(defvar cli-config-dir
  (cli-path-join
   (or (getenv "XDG_CONFIG_HOME") "~/.config") "org-export")
  "Directory for config files")

(defvar cli-config-file
  (cli-path-join cli-config-dir "config.el")
  "File for user config")

(defvar cli-packages '(htmlize request buttercup)
  "elisp packages installed by each script")

;; store the execution path for the current environment and provide it
;; to sh code blocks - otherwise, some system directories are
;; prepended in the code block's environment. Would be nice to figure
;; out where these are coming from. This solves the problem for shell
;; code blocks, but not for other languages (like python).
(defvar cli-sh-src-prologue
  (format "export PATH=\"%s\"" (mapconcat 'identity exec-path ":")))

(defun cli-option-p (str)
  "Return t if str is not nil and starts with '--'"
  (and str (string-equal (substring str 0 2) "--")))

(defun cli-opt-name (str)
  "Return the option name (strips leading '--')"
  (substring str 2 nil))

(defun cli-get-clargs ()
  "Return a list of args in `command-line-args' following '--'"
  (let ((dashpos (cl-position "--" command-line-args :test 'equal)))
    (if dashpos
        (nthcdr (+ dashpos 1) command-line-args)
      (error "Emacs must be invoked with '--' before script arguments."))))

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
"

  (let ((clargs (or arguments (cli-get-clargs)))
        (args (make-hash-table :test 'equal))
        (required (make-hash-table :test 'equal)))

    (if (or (member "-h" clargs) (member "--help" clargs))
	(progn (cli-show-help options-alist docstring)
	       (signal 'quit nil)))

    ;; assign starting values to hash table args; required items in `required'
    ;; have a value of t
    (mapc (lambda (optdef)
            (let ((name nil))
              (setq name (cli-opt-name (car optdef)))
              (puthash name (nth 2 optdef) args)
              (if (eq (length optdef) 2) (puthash name t required))
              )) options-alist)

    ;; iterate over command line arguments, adding values to args
    (while clargs
      (let ((name nil))
        (if (cli-option-p (car clargs))
            (progn
              ;; consume the first argument and add it to the hashmap
              (setq name (cli-opt-name (pop clargs)))

              (if (string-equal "invalid" (gethash name args "invalid"))
                  (error "The option '--%s' is not valid." name))

              (puthash
               name
               ;; if the next arg is an option name or this is the end of the
               ;; arguments, treat as a boolean option and assign a value of t
               (if (or (cli-option-p (car clargs)) (= (length clargs) 0))
                   t (pop clargs))
               args)

              ;; look ahead and raise an error if the next argument is not an option
              (if (and clargs (not (cli-option-p (car clargs))))
                  (error "The argument '%s' is not allowed here." (car clargs)))
              ))
        ))

    ;; check for required arguments
    (maphash (lambda (key val)
               (unless (gethash key args)
                 (error "The option '--%s' is required." key))
               ) required)

    args
    ))

(defun cli-break-string (str maxwidth)
  "Given a string `str', return a list of substrings, each no more
than `maxwidth' characters."
  (let* ((spl (split-string str))
         (chunks nil)
         (chunk (pop spl))
         (chars (length chunk))
         (thislen chars))
    (while spl
      (setq thislen (length (car spl)))
      (cond
       ;; if the length of the current chunk plus the next word does not exceed
       ;; the length limit, add another word
       ((< (+ chars thislen) maxwidth)
        (setq chunk (concat chunk (if (eq chunk "") "" " ") (pop spl)))
        (setq chars (length chunk)))
       ;; handle the edge case of a single word longer than the length limit
       ((>= thislen maxwidth)
        (setq chunks (append chunks `(,chunk)))
        (setq chunk (pop spl))
        (setq chars (length chunk)))
       ;; start a new chunk
       (t
        (setq chunks (append chunks `(,chunk)))
        (setq chunk "")
        (setq chars 0))
       ))
    ;; handle any residual words
    (append chunks `(,chunk))))

(defun cli-print-option (fstr option docstring doc-width)
  (let ((chunks (cli-break-string docstring doc-width)))
    (princ (format fstr option (car chunks)))
    (mapc (lambda (chunk) (princ (format fstr "" chunk)))
          (cdr chunks))))

(defun cli-show-help (options-alist &optional docstring)
  "Display options, defaults and help text defined in
`options-alist' (see `cli-parse-args' for specification)"
  (let* ((arg-width
	  (apply 'max
                 (mapcar (lambda (opt) (length (car opt))) options-alist)))
	 (fstr (format " %%-%ss  %%s\n" arg-width))
         (doc-width 55))
    (princ "Command line options:\n\n")
    (mapc (lambda (opt)
            (cli-print-option fstr (nth 0 opt) (nth 1 opt) doc-width))
          options-alist)
    (if docstring
        (progn
          (princ "\n")
          (princ (mapconcat 'identity (cli-break-string docstring 70) "\n"))
          (princ "\n")))
    (princ "\n")
    ))

(defun cli-package-setup (emacs-directory &optional package-list)
  "Set `user-emacs-directory' to `emacs-directory', activate
package, and install packages in `package-list' if provided."

  (unless (file-readable-p emacs-directory)
    (message (format "Creating directory %s" emacs-directory))
    (make-directory emacs-directory t))

  ;; must assign `user-emacs-directory' *before* initializing `package'
  (setq user-emacs-directory emacs-directory)
  (setq package-user-dir emacs-directory)
  (package-initialize)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

  (if package-list
      (mapc (lambda (pkg)
              (unless (package-installed-p pkg) (message "Installing %s" pkg))
              (let ((inhibit-message t))
                (package-install pkg))
              ) package-list)))

;; other utilities

(defun cli-eval-expr (instr)
  "evaluate the input string in a temporary buffer for side effects"
  (if instr
      (with-temp-buffer
        (insert instr)
        (eval-buffer))))

(defun cli-eval-file (file-path)
  "evaluate the specified file in a temporary buffer for side effects"
  (if (and file-path (file-exists-p file-path))
      (with-temp-buffer
        (message "loading %s" file-path)
        (insert-file-contents file-path)
        (eval-buffer))))

(defun cli-replace-all (from-str to-str)
  "Replace all occurrences of from-str with to-str in current buffer"
  (beginning-of-buffer)
  (while (search-forward from-str nil t)
    (replace-match to-str nil t)))

(defun cli-get-output-file (fname otherfile suffix)
  "Return expanded, absolute path of `fname', or if `fname' is nil,
`otherfile' with its suffix replaced by `suffix'"
  (let ((outfile
         (or fname (concat (file-name-sans-extension otherfile) suffix))))
    (expand-file-name outfile)))

(defvar cli-org-babel-languages-default
  '("R" "dot" "emacs-lisp" "latex" "python" "shell" "sql" "sqlite")
  "Startng values for list of languages for org-babel code blocks")

(defun cli-org-babel-load-languages (extra-langs)
  "Load languages named in `cli-org-babel-languages-default' plus
any identified in comma-delimited string `extra-langs'"
  (let* ((extras (if extra-langs (split-string extra-langs ",")))
         (language-names
          (append cli-org-babel-languages-default extras)))

    (org-babel-do-load-languages
     'org-babel-load-languages
     (mapcar (lambda (lang) `(,(make-symbol lang) . t)) language-names))
    ))


;;;;;;;;;;;;;;;;;;;;
;; Getting syntax highlighting to work seemed impossible until I came across
;; this emacs stackexchange post by Tobias:
;; https://emacs.stackexchange.com/questions/38437/org-mode-batch-export-missing-syntax-highlighting
;; The following code is taken from that post. Full disclosure: I really have
;; little idea what this does, but it does seem to enable syntax highlighting.

;; (require 'font-lock)
(require 'subr-x) ;; for `when-let'

(unless (boundp 'maximal-integer)
  (defconst maximal-integer (lsh -1 -1)
    "Maximal integer value representable natively in emacs lisp."))

(defun face-spec-default (spec)
  "Get list containing at most the default entry of face SPEC.
Return nil if SPEC has no default entry."
  (let* ((first (car-safe spec))
         (display (car-safe first)))
    (when (eq display 'default)
      (list (car-safe spec)))))

(defun face-spec-min-color (display-atts)
  "Get min-color entry of DISPLAY-ATTS pair from face spec."
  (let* ((display (car-safe display-atts)))
    (or (car-safe (cdr (assoc 'min-colors display)))
        maximal-integer)))

(defun face-spec-highest-color (spec)
  "Search face SPEC for highest color.
That means the DISPLAY entry of SPEC
with class 'color and highest min-color value."
  (let ((color-list (cl-remove-if-not
                     (lambda (display-atts)
                       (when-let ((display (car-safe display-atts))
                                  (class (and (listp display)
                                              (assoc 'class display)))
                                  (background (assoc 'background display)))
                         (and (member 'light (cdr background))
                              (member 'color (cdr class)))))
                     spec)))
    (cl-reduce (lambda (display-atts1 display-atts2)
                 (if (> (face-spec-min-color display-atts1)
                        (face-spec-min-color display-atts2))
                     display-atts1
                   display-atts2))
               (cdr color-list)
               :initial-value (car color-list))))

(defun face-spec-t (spec)
  "Search face SPEC for fall back."
  (cl-find-if (lambda (display-atts)
                (eq (car-safe display-atts) t))
              spec))

(defun my-face-attribute (face attribute &optional frame inherit)
  "Get FACE ATTRIBUTE from `face-user-default-spec' and not from `face-attribute'."
  (let* ((face-spec (face-user-default-spec face))
         (display-attr (or (face-spec-highest-color face-spec)
                           (face-spec-t face-spec)))
         (attr (cdr display-attr))
         (val (or (plist-get attr attribute) (car-safe (cdr (assoc attribute attr))))))
    ;; (message "attribute: %S" attribute) ;; for debugging
    (when (and (null (eq attribute :inherit))
               (null val))
      (let ((inherited-face (my-face-attribute face :inherit)))
        (when (and inherited-face
                   (null (eq inherited-face 'unspecified)))
          (setq val (my-face-attribute inherited-face attribute)))))
    ;; (message "face: %S attribute: %S display-attr: %S, val: %S" face attribute display-attr val) ;; for debugging
    (or val 'unspecified)))

(advice-add 'face-attribute :override #'my-face-attribute)
;;;; end code by Tobias

;; only executed if this is the script called from the command line
;; (like python's "if __name__ == '__main__'")
(if (member (file-name-nondirectory load-file-name)
	    (mapcar 'file-name-nondirectory command-line-args))
    (progn
      (setq options-alist
	    `(("--show-package-dir" "Print the path to package-dir" nil)
              ("--rm-package-dir" "Remove 'package-dir' and any installed packges" nil)
              ("--show-default-languages" "list the languages that are activated by default" nil)
	      ))

      (defvar docstring "\nManage and view packages and defaults\n")

      (condition-case err
          (setq args (cli-parse-args options-alist docstring))
        (quit (kill-emacs 0))
        (error (progn (message (nth 1 err)) (kill-emacs 1))))

      (defun getopt (name) (gethash name args))
      (cli-eval-file cli-config-file)
      (cli-package-setup cli-package-dir cli-packages)

      (if (getopt "show-package-dir")
          (progn
            (message cli-package-dir)
            (kill-emacs 0)))

      (if (and (getopt "rm-package-dir")
               (yes-or-no-p (format "Remove %s ? " cli-package-dir)))
          (progn
            (delete-directory cli-package-dir t)
            (kill-emacs 0)))

      (if (getopt "show-default-languages")
          (progn
            (print cli-org-babel-languages-default)
            (kill-emacs 0)))
      ))
