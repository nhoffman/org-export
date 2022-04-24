;; emacs --batch --load buttercup-tests.el

(setq lexical-binding t)
(require 'cli (concat (file-name-directory load-file-name) "org-export-cli.el"))
(cli-el-get-setup cli-package-dir '(buttercup))
(require 'cl-lib)
(require 'buttercup)

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
	("--css" "Path or URL of css stylesheet" nil)
        ("--css-integrity"
         "Optional value for css link integrity attribute" nil)
	("--embed-css" "Include contents of css in a <style> block" nil)
	("--bootstrap"
         "Make Bootstrap-specific modifications to html output;
          if selected, link to Bootstrap CDN by default"
         nil)
	("--package-dir"
         "Directory containing elpa packages" ,cli-package-dir)
        ("--config"
         "An elisp expression defining additional configuration" nil)
        ("--config-file"
         "A file path providing  additional configuration" nil)
	))


(let* ((clargs '("--infile" "foo" "--evaluate" "--embed-css")))
  (setq args (cli-parse-args options-alist "" clargs))
  (describe "cli-parse-args with valid arguments"
    (it "tests --evaluate"
      (expect (gethash "evaluate" args) :to-be t))
    (it "tests --embed-css"
      (expect (gethash "embed-css" args) :to-be t))
    (it "tests --infile"
      (expect (gethash "infile" args) :to-equal "foo"))
    (it "tests --outfile"
      (expect (gethash "outfile" args) :to-be nil))
    ))

(describe "cli-parse-args with invalid arguments"
  (it "missing --infile"
    (expect (cli-parse-args options-alist "" '("--evaluate")) :to-throw))
  (it "multiple values for one argument"
    (expect (cli-parse-args options-alist "" '("--infile" "foo" "bar")) :to-throw))
  (it "invalid argument"
    (expect (cli-parse-args options-alist "" '("--foo")) :to-throw))
  )

(describe "various utility functions"
  (it "cli-option-p is t"
    (expect (cli-option-p "--infile") :to-be t))
  (it "cli-option-p is nil"
    (expect (cli-option-p "infile") :to-be nil))
  (it "cli-option-p is nil given nil"
    (expect (cli-option-p nil) :to-be nil))
  (it "cli-opt-name"
    (expect (cli-opt-name "--infile") :to-equal "infile"))
  )

(describe "tests of cli-break-string"
  (it "single word"
    (expect (cli-break-string "word" 10) :to-equal '("word")))
  (it "two words"
    (expect (cli-break-string "two words" 10) :to-equal '("two words")))
  (it "a longer phrase"
    (expect (cli-break-string "this is a longer phrase" 15)
            :to-equal '("this is a" "longer phrase")))
  (it "a word exactly maxwidth"
    (expect (cli-break-string "abcdefghij" 10) :to-equal '("abcdefghij")))
  (it "a word maxwidth + 1"
    (expect (cli-break-string "abcdefghijk" 10) :to-equal '("abcdefghijk")))
  (it "two long words"
    (expect (cli-break-string "abcdefghijk lmnopqrstuv" 10)
            :to-equal '("abcdefghijk" "lmnopqrstuv")))
  (it "three long words"
    (expect (cli-break-string "abcdefghijk lmnopqrstuv 12345678910" 10)
            :to-equal '("abcdefghijk" "lmnopqrstuv" "12345678910")))
  (it "long short long"
    (expect (cli-break-string "abcdefghijk lmnopq 12345678910" 10)
            :to-equal '("abcdefghijk" "lmnopq" "12345678910")))
  )

(buttercup-run)
