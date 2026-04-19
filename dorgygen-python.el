;;; dorgygen-python.el --- Python support for dorgygen  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stefano Ghirlanda

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Python language support for dorgygen.  Uses docstrings as the
;; documentation source.  Load with (require 'dorgygen-python).

;;; Code:

(require 'dorgygen)
(require 'treesit)
(require 'seq)

;;; Helpers

(defun dorgygen--python-body (node)
  "Return the body block of function or class NODE, or nil."
  (treesit-node-child-by-field-name node "body"))

(defun dorgygen--python-clean-docstring (text)
  "Strip triple-quote delimiters and normalize TEXT as a docstring."
  (let* ((text (replace-regexp-in-string
                "\\`[[:space:]]*\"\"\"\\|\"\"\"[[:space:]]*\\'" "" text))
         (text (replace-regexp-in-string
                "\\`[[:space:]]*'''\\|'''[[:space:]]*\\'" "" text))
         (lines (split-string (string-trim text) "\n"))
         (lines (mapcar #'string-trim lines))
         (lines (seq-filter (lambda (l) (not (string-empty-p l))) lines))
         (result (string-join lines " ")))
    (when (not (string-empty-p result))
      (setq result (concat (upcase (substring result 0 1))
                           (substring result 1)))
      (unless (string-match-p "[.!?]$" result)
        (setq result (concat result ".")))
      result)))

(defun dorgygen--python-docstring (node)
  "Extract docstring text from function or class NODE, or nil."
  (when-let* ((body (dorgygen--python-body node))
              (first (car (treesit-node-children body t)))
              (_ (equal "expression_statement" (treesit-node-type first)))
              (str-node (treesit-node-child first 0 t))
              (_ (equal "string" (treesit-node-type str-node))))
    (dorgygen--python-clean-docstring (treesit-node-text str-node t))))

(defun dorgygen--python-param-name (param)
  "Extract parameter name string from PARAM node, or nil."
  (pcase (treesit-node-type param)
    ("identifier"
     (treesit-node-text param t))
    ("typed_parameter"
     (treesit-node-text (treesit-node-child param 0 t) t))
    ((or "default_parameter" "typed_default_parameter")
     (treesit-node-text
      (treesit-node-child-by-field-name param "name") t))
    (_ nil)))

(defun dorgygen--python-param-type (param)
  "Extract type annotation text from PARAM node, or nil."
  (when-let* ((type-node (treesit-node-child-by-field-name param "type")))
    (treesit-node-text type-node t)))

(defun dorgygen--python-param-default (param)
  "Extract default value text from PARAM node, or nil."
  (when-let* ((val (treesit-node-child-by-field-name param "value")))
    (treesit-node-text val t)))

(defun dorgygen--python-is-method-p (node)
  "Return t if function NODE is a method inside a class body."
  (let* ((parent (treesit-node-parent node))
         (ptype (treesit-node-type parent)))
    (or (and (equal "block" ptype)
             (equal "class_definition"
                    (treesit-node-type (treesit-node-parent parent))))
        (and (equal "decorated_definition" ptype)
             (let ((gp (treesit-node-parent parent)))
               (and (equal "block" (treesit-node-type gp))
                    (equal "class_definition"
                           (treesit-node-type (treesit-node-parent gp)))))))))

;;; Shared insertion

(defun dorgygen--python-insert-assignment (node)
  "Insert a list item for assignment NODE if followed by a docstring."
  (let* ((parent (treesit-node-parent node))
         (next (treesit-node-next-sibling parent t))
         (str-node (when (and next
                              (equal "expression_statement"
                                     (treesit-node-type next)))
                     (treesit-node-child next 0 t))))
    (when (and str-node (equal "string" (treesit-node-type str-node)))
      (when-let* ((left (treesit-node-child-by-field-name node "left"))
                  (name (treesit-node-text left t))
                  (doc (dorgygen--python-clean-docstring
                        (treesit-node-text str-node t))))
        (insert (format "- ~%s~ :: %s\n" name doc))))))

(defun dorgygen--python-insert-function (node levl &optional is-method)
  "Insert heading and docs for function NODE at level LEVL.
If IS-METHOD is non-nil, skip the node when it has no docstring.
Returns the heading string, or nil if skipped."
  (let* ((doc (dorgygen--python-docstring node))
         (name (treesit-node-text
                (treesit-node-child-by-field-name node "name") t))
         (hdn (dorgygen--heading name))
         (params (treesit-node-child-by-field-name node "parameters"))
         (ret (treesit-node-child-by-field-name node "return_type"))
         (exis (org-find-exact-headline-in-buffer hdn)))
    (when (or doc (not is-method))
      (if (not exis)
          (insert levl " " hdn "\n\n")
        (goto-char exis)
        (forward-line)
        (dorgygen--delete-non-user-content))
      (when doc
        (insert "- " doc "\n"))
      (when params
        (dolist (param (treesit-node-children params t))
          (let ((pname (dorgygen--python-param-name param)))
            (when (and pname (not (member pname '("self" "cls"))))
              (let ((ptype (dorgygen--python-param-type param))
                    (pdefault (dorgygen--python-param-default param)))
                (insert (format "- param ~%s~%s%s\n"
                                pname
                                (if ptype (format " : %s" ptype) "")
                                (if pdefault
                                    (format " = %s" pdefault) ""))))))))
      (when ret
        (insert (format "- returns : %s\n"
                        (treesit-node-text ret t))))
      (insert "\n")
      hdn)))

;;; Public handlers

(defun dorgygen--python-assignment (node)
  "Document module-level assignment NODE."
  (let* ((parent (treesit-node-parent node))
         (gp (treesit-node-parent parent)))
    (when (equal "module" (treesit-node-type gp))
      (dorgygen--python-insert-assignment node))))

(defun dorgygen--python-function (node levl)
  "Document top-level function NODE at org level LEVL."
  (unless (dorgygen--python-is-method-p node)
    (dorgygen--python-insert-function node levl)))

(defun dorgygen--python-class (node levl)
  "Document class NODE at org level LEVL.
Returns a list of heading strings (class + documented methods)."
  (let* ((name (treesit-node-text
                (treesit-node-child-by-field-name node "name") t))
         (hdn (dorgygen--heading name))
         (exis (org-find-exact-headline-in-buffer hdn))
         (method-levl (concat levl "*"))
         (body (dorgygen--python-body node))
         (result (list hdn)))
    (if (not exis)
        (insert levl " " hdn "\n\n")
      (goto-char exis)
      (forward-line)
      (dorgygen--delete-non-user-content))
    (when-let ((doc (dorgygen--python-docstring node)))
      (insert "- " doc "\n"))
    (when body
      (dolist (child (treesit-node-children body t))
        (pcase (treesit-node-type child)
          ("expression_statement"
           (let ((inner (treesit-node-child child 0 t)))
             (when (and inner (equal "assignment" (treesit-node-type inner)))
               (dorgygen--python-insert-assignment inner))))
          ("function_definition"
           (when-let ((mhdn (dorgygen--python-insert-function
                             child method-levl t)))
             (push mhdn result)))
          ("decorated_definition"
           (when-let* ((func (car (dorgygen--find
                                   "function_definition" child)))
                       (mhdn (dorgygen--python-insert-function
                              func method-levl t)))
             (push mhdn result))))))
    (insert "\n")
    result))

;;; Registration

(dorgygen-add-language 'python
  :extensions '("py")
  :comments '("^\"\"\"[ \t]*" "\"\"\"$" "^'''[ \t]*" "'''$" "^#[ \t]*")
  :file-level '(("assignment" . dorgygen--python-assignment))
  :subheading '(("function_definition" . dorgygen--python-function)
                ("class_definition"    . dorgygen--python-class)))

(provide 'dorgygen-python)

;;; dorgygen-python.el ends here
