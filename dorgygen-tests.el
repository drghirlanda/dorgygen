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

(provide 'dorgygen-tests)

;;; dorgygen-tests.el ends here
