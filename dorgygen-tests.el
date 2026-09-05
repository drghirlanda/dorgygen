;;; dorgygen-tests.el --- ERT tests for dorgygen  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stefano Ghirlanda

;;; Commentary:

;; Run interactively with M-x ert RET t RET, or from the shell:
;;
;;   emacs --batch -l ert -l dorgygen.el -l dorgygen-python.el \
;;         -l dorgygen-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'dorgygen)
(require 'dorgygen-c)
(require 'dorgygen-cpp)
(require 'dorgygen-python)

;;; Test helper

(defun dorgygen-test--run (source mode node-type handler &rest handler-args)
  "Call HANDLER on the first NODE-TYPE node found in SOURCE.
SOURCE is inserted into a temporary buffer with MODE active.
HANDLER-ARGS are passed after the node.  Returns the string
inserted into a fresh org-mode output buffer."
  (let ((src (generate-new-buffer " *dorgygen-test*"))
        result)
    (unwind-protect
        (progn
          (with-current-buffer src
            (insert source)
            (funcall mode))
          (let* ((root (treesit-parser-root-node
                        (car (treesit-parser-list src))))
                 (node (car (dorgygen--find node-type root))))
            (with-temp-buffer
              (org-mode)
              (apply handler node handler-args)
              (setq result (buffer-string)))))
      (kill-buffer src))
    result))

;;; dorgygen--delete-non-user-content

(defun dorgygen-test--heading-pos (heading)
  "Return position of the line beginning HEADING in current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward heading)
    (line-beginning-position)))

(ert-deftest dorgygen-delete-non-user-content/empty-last-heading ()
  "Empty heading at end of buffer must not signal \"wrong side of point\"."
  (with-temp-buffer
    (org-mode)
    (insert "* top\n** ~file.el~\n")
    (let ((hp (dorgygen-test--heading-pos "** ~file.el~")))
      (dorgygen--delete-non-user-content hp)
      (insert "- item\n")
      (should (string-match-p "- item" (buffer-string))))))

(ert-deftest dorgygen-delete-non-user-content/empty-heading-followed-by-another ()
  "Empty heading must not eat content from the following heading's subtree."
  (with-temp-buffer
    (org-mode)
    (insert "* top\n** ~file.el~\n** ~other.el~\n- keep\n")
    (let ((hp (dorgygen-test--heading-pos "** ~file.el~")))
      (dorgygen--delete-non-user-content hp)
      (should (string-match-p "- keep" (buffer-string))))))

(ert-deftest dorgygen-delete-non-user-content/preserves-user-prose ()
  "User prose between heading and first list item is preserved."
  (with-temp-buffer
    (org-mode)
    (insert "* top\n** ~file.el~\nSome prose.\n- old\n** ~other.el~\n")
    (let ((hp (dorgygen-test--heading-pos "** ~file.el~")))
      (dorgygen--delete-non-user-content hp)
      (should (string-match-p "Some prose" (buffer-string)))
      (should-not (string-match-p "- old" (buffer-string))))))

(ert-deftest dorgygen-delete-non-user-content/preserves-subheading ()
  "Subheading and its content are preserved when deleting list items."
  (with-temp-buffer
    (org-mode)
    (insert "* top\n** ~file.el~\n- old\n*** ~func~\ncontent\n")
    (let ((hp (dorgygen-test--heading-pos "** ~file.el~")))
      (dorgygen--delete-non-user-content hp)
      (should-not (string-match-p "- old" (buffer-string)))
      (should (string-match-p "\\*\\*\\* ~func~" (buffer-string)))
      (should (string-match-p "content" (buffer-string))))))

;;; dorgygen--format-comment

(ert-deftest dorgygen-format-comment/single-line ()
  (should (equal (dorgygen--format-comment "Hello world")
                 "Hello world")))

(ert-deftest dorgygen-format-comment/continuation-line ()
  (should (equal (dorgygen--format-comment "First\nSecond")
                 "First\n  Second")))

(ert-deftest dorgygen-format-comment/indented-become-items ()
  (should (equal (dorgygen--format-comment "Description\n  item one\n  item two")
                 "Description\n  - item one\n  - item two")))

(ert-deftest dorgygen-format-comment/blank-lines-ignored ()
  (should (equal (dorgygen--format-comment "Hello\n\nWorld")
                 "Hello\n  World")))

(ert-deftest dorgygen-format-comment/mixed ()
  (should (equal (dorgygen--format-comment "Top\n  sub\ncont")
                 "Top\n  - sub\n  cont")))

;;; C: dorgygen--c-type_definition

(ert-deftest dorgygen-c-typedef/with-comment ()
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "// a new type\ntypedef float my_float;\n"
              #'c-ts-mode "type_definition" #'dorgygen--c-type_definition)))
    (should (string-match-p "~typedef float my_float~" out))
    (should (string-match-p "a new type" out))))

(ert-deftest dorgygen-c-typedef/without-comment ()
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "typedef float my_float;\n"
              #'c-ts-mode "type_definition" #'dorgygen--c-type_definition)))
    (should (string-match-p "~typedef float my_float~" out))
    (should-not (string-match-p "\\.\\." out))))

;;; C: dorgygen--c-declaration

(ert-deftest dorgygen-c-declaration/basic ()
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "// compute sum\nint sum(int a, int b);\n"
              #'c-ts-mode "declaration" #'dorgygen--c-declaration "**")))
    (should (string-match-p "\\*\\* ~sum~" out))
    (should (string-match-p "compute sum" out))
    (should (string-match-p "In:.*~int a~" out))
    (should (string-match-p "In:.*~int b~" out))
    (should (string-match-p "Out:.*~int~" out))))

(ert-deftest dorgygen-c-declaration/no-comment ()
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "int f(int x);\n"
              #'c-ts-mode "declaration" #'dorgygen--c-declaration "**")))
    (should (string-match-p "~f~" out))
    (should (string-match-p "In:.*~int x~" out))
    (should (string-match-p "Out:.*~int~" out))))

(ert-deftest dorgygen-c-declaration/multiline-comment ()
  "Multiple // lines must appear on separate lines, not merged."
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "// first line\n// second line\nvoid f(void);\n"
              #'c-ts-mode "declaration" #'dorgygen--c-declaration "**")))
    (should (string-match-p "first line" out))
    (should (string-match-p "second line" out))
    (should (string-match-p "first line\n" out))))

(ert-deftest dorgygen-c-declaration/capitalises-only-the-first-line ()
  "A sentence wrapping across // lines keeps its lower-case continuation.
Each // line is a separate treesit node, so capitalising every node
turned a wrapped sentence into \"first half\\nSecond half\"."
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "// the gradient applies to\n// length size, evaluated at\nvoid f(void);\n"
              #'c-ts-mode "declaration" #'dorgygen--c-declaration "**"))
        (case-fold-search nil))          ; the whole point here is the case
    (should (string-match-p "The gradient applies to" out))
    (should (string-match-p "length size, evaluated at" out))
    (should-not (string-match-p "Length size" out))))

(ert-deftest dorgygen-c-declaration/capitalises-a-block-comment-once ()
  "A block comment was already one node; it must keep behaving that way."
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "/* the first word\n   continues here */\nvoid f(void);\n"
              #'c-ts-mode "declaration" #'dorgygen--c-declaration "**"))
        (case-fold-search nil))
    (should (string-match-p "The first word" out))
    (should (string-match-p "continues here" out))))

(ert-deftest dorgygen-c-declaration/indented-items ()
  "// lines indented with extra spaces become org sub-list items."
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "// description\n//   item one\n//   item two\nvoid f(void);\n"
              #'c-ts-mode "declaration" #'dorgygen--c-declaration "**")))
    (should (string-match-p "description" out))
    (should (string-match-p "  - item one" out))
    (should (string-match-p "  - item two" out))))

(ert-deftest dorgygen-c-declaration/blank-comment-transparent ()
  "A blank // between real comments must not swallow them."
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "// real comment\n//\nvoid f(void);\n"
              #'c-ts-mode "declaration" #'dorgygen--c-declaration "**")))
    (should (string-match-p "real comment" out))
    (should-not (string-match-p "- \\.$" out))))

(ert-deftest dorgygen-c-declaration/argument-comments ()
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "int f(int x, // the x\n      int y  // the y\n      );\n"
              #'c-ts-mode "declaration" #'dorgygen--c-declaration "**")))
    (should (string-match-p "the x" out))
    (should (string-match-p "the y" out))))

(ert-deftest dorgygen-c-declaration/multiline-argument-comment ()
  (skip-unless (treesit-language-available-p 'c))
  (let ((out (dorgygen-test--run
              "int f(int x  // first line\n              // second line\n      );\n"
              #'c-ts-mode "declaration" #'dorgygen--c-declaration "**")))
    (should (string-match-p "first line" out))
    (should (string-match-p "second line" out))
    (should (string-match-p "first line\n" out))))

;;; Python: dorgygen--python-function

(ert-deftest dorgygen-python-function/typed ()
  (skip-unless (treesit-language-available-p 'python))
  (let ((out (dorgygen-test--run
              "def process(items: list[str], n: int = 10) -> bool:\n    \"\"\"Process items.\"\"\"\n    pass\n"
              #'python-ts-mode "function_definition"
              #'dorgygen--python-function "**")))
    (should (string-match-p "~process~" out))
    (should (string-match-p "Process items" out))
    (should (string-match-p "param ~items~ : list" out))
    (should (string-match-p "param ~n~ : int = 10" out))
    (should (string-match-p "returns : bool" out))))

(ert-deftest dorgygen-python-function/no-docstring ()
  (skip-unless (treesit-language-available-p 'python))
  (let ((out (dorgygen-test--run
              "def f(x):\n    pass\n"
              #'python-ts-mode "function_definition"
              #'dorgygen--python-function "**")))
    (should (string-match-p "~f~" out))
    (should (string-match-p "param ~x~" out))
    (should-not (string-match-p "returns" out))))

(ert-deftest dorgygen-python-function/no-params ()
  (skip-unless (treesit-language-available-p 'python))
  (let ((out (dorgygen-test--run
              "def greet():\n    \"\"\"Say hello.\"\"\"\n    pass\n"
              #'python-ts-mode "function_definition"
              #'dorgygen--python-function "**")))
    (should (string-match-p "~greet~" out))
    (should (string-match-p "Say hello" out))
    (should-not (string-match-p "param" out))))

;;; Python: dorgygen--python-class

(ert-deftest dorgygen-python-class/full ()
  (skip-unless (treesit-language-available-p 'python))
  (let ((out (dorgygen-test--run
              (concat "class Buffer:\n"
                      "    \"\"\"A fixed-size buffer.\"\"\"\n\n"
                      "    capacity = 0\n"
                      "    \"\"\"Maximum items.\"\"\"\n\n"
                      "    def push(self, item: str) -> None:\n"
                      "        \"\"\"Add an item.\"\"\"\n"
                      "        pass\n")
              #'python-ts-mode "class_definition"
              #'dorgygen--python-class "**")))
    (should (string-match-p "~Buffer~" out))
    (should (string-match-p "fixed-size buffer" out))
    (should (string-match-p "~capacity~ ::" out))
    (should (string-match-p "Maximum items" out))
    (should (string-match-p "~push~" out))
    (should (string-match-p "Add an item" out))
    (should (string-match-p "param ~item~ : str" out))
    (should (string-match-p "returns : None" out))))

(ert-deftest dorgygen-python-class/undocumented-method-skipped ()
  (skip-unless (treesit-language-available-p 'python))
  (let ((out (dorgygen-test--run
              (concat "class C:\n"
                      "    \"\"\"A class.\"\"\"\n\n"
                      "    def undoc(self):\n"
                      "        pass\n")
              #'python-ts-mode "class_definition"
              #'dorgygen--python-class "**")))
    (should (string-match-p "~C~" out))
    (should-not (string-match-p "~undoc~" out))))

;;; Python: dorgygen--python-assignment

(ert-deftest dorgygen-python-assignment/with-docstring ()
  (skip-unless (treesit-language-available-p 'python))
  (let ((out (dorgygen-test--run
              "MAX_SIZE = 100\n\"\"\"Maximum buffer size.\"\"\"\n"
              #'python-ts-mode "assignment"
              #'dorgygen--python-assignment)))
    (should (string-match-p "~MAX_SIZE~" out))
    (should (string-match-p "Maximum buffer size" out))))

(ert-deftest dorgygen-python-assignment/without-docstring ()
  (skip-unless (treesit-language-available-p 'python))
  (let ((out (dorgygen-test--run
              "IGNORED = 42\n"
              #'python-ts-mode "assignment"
              #'dorgygen--python-assignment)))
    (should (string-empty-p out))))

;;; C++: dorgygen--cpp-class

(ert-deftest dorgygen-cpp-class/with-comment ()
  (skip-unless (treesit-language-available-p 'cpp))
  (let ((out (dorgygen-test--run
              "// A buffer class\nclass Buffer {\n};\n"
              #'c++-ts-mode "class_specifier"
              #'dorgygen--cpp-class "**")))
    (should (string-match-p "~Buffer~" out))
    (should (string-match-p "buffer class" out))))

(ert-deftest dorgygen-cpp-class/field-comment-before ()
  (skip-unless (treesit-language-available-p 'cpp))
  (let ((out (dorgygen-test--run
              "class Buffer {\n  // capacity of buffer\n  int capacity;\n};\n"
              #'c++-ts-mode "class_specifier"
              #'dorgygen--cpp-class "**")))
    (should (string-match-p "~int capacity~" out))
    (should (string-match-p "capacity of buffer" out))))

(ert-deftest dorgygen-cpp-class/field-comment-after ()
  "Trailing // comments on field lines are also picked up."
  (skip-unless (treesit-language-available-p 'cpp))
  (let ((out (dorgygen-test--run
              "class Buffer {\n  int capacity; // capacity of buffer\n};\n"
              #'c++-ts-mode "class_specifier"
              #'dorgygen--cpp-class "**")))
    (should (string-match-p "~int capacity~" out))
    (should (string-match-p "capacity of buffer" out))))

(ert-deftest dorgygen-cpp-class/with-method ()
  (skip-unless (treesit-language-available-p 'cpp))
  (let ((out (dorgygen-test--run
              "class Buffer {\n  // add item\n  void push(int item);\n};\n"
              #'c++-ts-mode "class_specifier"
              #'dorgygen--cpp-class "**")))
    (should (string-match-p "~Buffer~" out))
    (should (string-match-p "~push~" out))
    (should (string-match-p "add item" out))
    (should (string-match-p "In:.*~int item~" out))
    (should (string-match-p "Out:.*~void~" out))))

(ert-deftest dorgygen-cpp-class/inline-method ()
  "Methods defined inline (with body) are also documented."
  (skip-unless (treesit-language-available-p 'cpp))
  (let ((out (dorgygen-test--run
              "class Buffer {\n  // get size\n  int size() { return 0; }\n};\n"
              #'c++-ts-mode "class_specifier"
              #'dorgygen--cpp-class "**")))
    (should (string-match-p "~size~" out))
    (should (string-match-p "get size" out))))

(ert-deftest dorgygen-cpp-struct/basic ()
  "struct_specifier uses the same handler as class_specifier."
  (skip-unless (treesit-language-available-p 'cpp))
  (let ((out (dorgygen-test--run
              "// A 2D point\nstruct Point {\n  int x;\n  int y;\n};\n"
              #'c++-ts-mode "struct_specifier"
              #'dorgygen--cpp-class "**")))
    (should (string-match-p "~Point~" out))
    (should (string-match-p "2D point" out))
    (should (string-match-p "~int x~" out))
    (should (string-match-p "~int y~" out))))

;;; dorgygen-elisp tests

(ert-deftest dorgygen-elisp-defun/basic ()
  "defun with docstring and params."
  (skip-unless (treesit-language-available-p 'elisp))
  (let ((out (dorgygen-test--run
              "(defun foo (x y)\n  \"Do something.\"\n  nil)\n"
              #'dorgygen--elisp-activate "function_definition"
              #'dorgygen--elisp-defun "**")))
    (should (string-match-p "~foo~" out))
    (should (string-match-p "Do something" out))
    (should (string-match-p "param ~x~" out))
    (should (string-match-p "param ~y~" out))))

(ert-deftest dorgygen-elisp-defun/optional-rest ()
  "&optional and &rest params labelled correctly."
  (skip-unless (treesit-language-available-p 'elisp))
  (let ((out (dorgygen-test--run
              "(defun bar (x &optional y &rest z)\n  \"Bar.\"\n  nil)\n"
              #'dorgygen--elisp-activate "function_definition"
              #'dorgygen--elisp-defun "**")))
    (should (string-match-p "param ~x~" out))
    (should (string-match-p "optional ~y~" out))
    (should (string-match-p "rest ~z~" out))))

(ert-deftest dorgygen-elisp-defun/no-docstring ()
  "defun without a docstring still gets a heading."
  (skip-unless (treesit-language-available-p 'elisp))
  (let ((out (dorgygen-test--run
              "(defun baz (a)\n  a)\n"
              #'dorgygen--elisp-activate "function_definition"
              #'dorgygen--elisp-defun "**")))
    (should (string-match-p "~baz~" out))
    (should (string-match-p "param ~a~" out))))

(ert-deftest dorgygen-elisp-defmacro/basic ()
  "defmacro is documented like defun."
  (skip-unless (treesit-language-available-p 'elisp))
  (let ((out (dorgygen-test--run
              "(defmacro my-macro (x)\n  \"A macro.\"\n  x)\n"
              #'dorgygen--elisp-activate "macro_definition"
              #'dorgygen--elisp-defun "**")))
    (should (string-match-p "~my-macro~" out))
    (should (string-match-p "A macro" out))
    (should (string-match-p "param ~x~" out))))

(ert-deftest dorgygen-elisp-variable/defvar ()
  "defvar with docstring becomes a file-level item."
  (skip-unless (treesit-language-available-p 'elisp))
  (let ((out (dorgygen-test--run
              "(defvar my-var 42\n  \"A variable.\")\n"
              #'dorgygen--elisp-activate "special_form"
              #'dorgygen--elisp-variable)))
    (should (string-match-p "~my-var~" out))
    (should (string-match-p "A variable" out))))

(ert-deftest dorgygen-elisp-variable/defconst ()
  "defconst is also documented."
  (skip-unless (treesit-language-available-p 'elisp))
  (let ((out (dorgygen-test--run
              "(defconst max-size 100\n  \"Maximum size.\")\n"
              #'dorgygen--elisp-activate "special_form"
              #'dorgygen--elisp-variable)))
    (should (string-match-p "~max-size~" out))
    (should (string-match-p "Maximum size" out))))

(ert-deftest dorgygen-elisp-variable/other-special-form-ignored ()
  "Non-defvar special forms produce no output."
  (skip-unless (treesit-language-available-p 'elisp))
  (let ((out (dorgygen-test--run
              "(setq x 1)\n"
              #'dorgygen--elisp-activate "special_form"
              #'dorgygen--elisp-variable)))
    (should (string-empty-p out))))

;;; dorgygen-use-languages tests

(ert-deftest dorgygen-use-languages/registers-language ()
  "dorgygen-use-languages registers the requested language."
  (dorgygen-use-languages 'c)
  (should (assq 'c dorgygen--language-alist)))

(ert-deftest dorgygen-use-languages/registers-multiple ()
  "dorgygen-use-languages accepts multiple language symbols."
  (dorgygen-use-languages 'c 'python 'cpp)
  (should (assq 'c      dorgygen--language-alist))
  (should (assq 'python dorgygen--language-alist))
  (should (assq 'cpp    dorgygen--language-alist)))

(ert-deftest dorgygen-use-languages/unknown-language ()
  "dorgygen-use-languages signals an error for unknown languages."
  (should-error (dorgygen-use-languages 'nosuchlang)))

;;; End-to-end tests

(defun dorgygen-test--in-project (org-body fn)
  "Run FN in a temporary project whose org file contains ORG-BODY.
The project has a src/ directory holding one header and a doc/
directory holding the org file, so that a DORG_REX of ../src/...
resolves the way it does in a real project.  FN is called with the
org buffer current; it should return a value, which is returned."
  (let* ((root (make-temp-file "dorgygen-e2e" t))
         (src  (expand-file-name "src" root))
         (doc  (expand-file-name "doc" root))
         (hdr  (expand-file-name "thing.h" src))
         (org  (expand-file-name "api.org" doc)))
    (unwind-protect
        (progn
          (make-directory src) (make-directory doc)
          (with-temp-file hdr
            (insert "// Do the thing.\n"
                    "int thing_do( int n );\n"))
          (with-temp-file org (insert org-body))
          (let ((buf (find-file-noselect org)))
            (unwind-protect
                (with-current-buffer buf
                  (org-mode)
                  (funcall fn))
              (kill-buffer buf))))
      (delete-directory root t))))

(defun dorgygen-test--regenerate ()
  "Run dorgygen on every heading of the current buffer that has DORG_REX."
  (let (positions)
    (org-map-entries
     (lambda () (when (org-entry-get (point) "DORG_REX") (push (point) positions))))
    ;; bottom-up, so that regenerating one section cannot move the next
    (dolist (pos positions)
      (widen)
      (goto-char pos)
      (dorgygen)))
  (widen))

(defconst dorgygen-test--e2e-folded-org
  "#+startup: overview
* API
** Section
:PROPERTIES:
:DORG_REX: ../src/^thing\\.h$
:END:

USER PROSE MUST SURVIVE.

** Later Section

LATER PROSE MUST SURVIVE.
"
  "As `dorgygen-test--e2e-org', but folded on startup and with a
following section, which is what exposes a bound computed from
visible headings.")

(ert-deftest dorgygen-e2e/folded-buffer-keeps-later-sections ()
  "Regenerating a folded buffer must not consume the sections that follow.

With #+startup: overview org folds the document, and a bound computed
with `org-next-visible-heading' skips every folded heading.  The bound
then lands past the end of the section and the deletion takes all the
sections in between with it."
  (dorgygen-use-languages 'c)
  (should
   (dorgygen-test--in-project
    dorgygen-test--e2e-folded-org
    (lambda ()
      ;; Generate once so that the file heading exists: the damage happens
      ;; on the path that deletes existing content, not the one that
      ;; inserts it for the first time.
      (dorgygen-test--regenerate)
      ;; Fold explicitly.  Batch Emacs does not apply #+startup: visibility,
      ;; so relying on it would leave this test passing for the wrong reason.
      (org-overview)
      (should (org-invisible-p (1- (point-max))))
      (dorgygen-test--regenerate)
      (let ((text (buffer-string)))
        (and (string-match-p "USER PROSE MUST SURVIVE" text)
             (string-match-p "LATER PROSE MUST SURVIVE" text)
             (string-match-p "Later Section" text)
             (string-match-p "thing_do" text)))))))

(defconst dorgygen-test--e2e-org
  "* API
** Section
:PROPERTIES:
:DORG_REX: ../src/^thing\\.h$
:END:

USER PROSE MUST SURVIVE.
"
  "An org file with one documented section and one line of user prose.")

(ert-deftest dorgygen-e2e/first-run-keeps-user-prose ()
  "A first run generates documentation without losing user prose."
  (dorgygen-use-languages 'c)
  (should
   (dorgygen-test--in-project
    dorgygen-test--e2e-org
    (lambda ()
      (dorgygen-test--regenerate)
      (and (string-match-p "USER PROSE MUST SURVIVE" (buffer-string))
           (string-match-p "thing_do" (buffer-string)))))))

(ert-deftest dorgygen-e2e/is-idempotent ()
  "Regenerating twice must not destroy user prose.

The first run inserts the generated block directly after the
property drawer, which pushes any user prose below the generated
list.  The second run then deletes from the first list item to the
end of the section, taking the prose with it."
  (dorgygen-use-languages 'c)
  (should
   (dorgygen-test--in-project
    dorgygen-test--e2e-org
    (lambda ()
      (dorgygen-test--regenerate)
      (dorgygen-test--regenerate)
      (string-match-p "USER PROSE MUST SURVIVE" (buffer-string))))))

(ert-deftest dorgygen-e2e/user-prose-stays-above-generated-content ()
  "User prose written under a heading stays above the generated list.

This is the invariant the idempotency failure violates: if prose is
relocated below the generated content on the first run, the second
run cannot tell it from generated content."
  (dorgygen-use-languages 'c)
  (should
   (dorgygen-test--in-project
    dorgygen-test--e2e-org
    (lambda ()
      (dorgygen-test--regenerate)
      (let ((prose (string-match "USER PROSE MUST SURVIVE" (buffer-string)))
            (gen   (string-match "^\\*\\*\\* " (buffer-string))))
        (and prose gen (< prose gen)))))))

(provide 'dorgygen-tests)

;;; dorgygen-tests.el ends here
