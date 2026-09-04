;;; dorgygen-cpp.el --- C++ support for dorgygen  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stefano Ghirlanda

;; This file is not part of GNU Emacs.

;;; Commentary:

;; C++ language support for dorgygen.  Reuses C comment handling and
;; function-declaration handler; adds a class/struct handler.
;;
;; Supported constructs:
;;   - Top-level function declarations and definitions
;;   - Typedefs
;;   - Classes and structs: class comment, field declarations,
;;     member function declarations and inline definitions
;;
;; Load with (require 'dorgygen-cpp).

;;; Code:

(require 'dorgygen)
(require 'dorgygen-c)

;;; Handlers

(defun dorgygen--cpp-field (node)
  "Insert a list item for C++ field declaration NODE.
Looks for a comment before the field, then after (for trailing comments)."
  (let* ((text (string-trim
                (string-replace ";" "" (treesit-node-text node t))))
         (com (or (dorgygen--comment-about node)
                  (dorgygen--comment-about node t))))
    (insert (format "- ~%s~.%s\n" text
                    (if com (concat " " (dorgygen--format-comment com)) "")))))

(defun dorgygen--cpp-class (node levl)
  "Document C++ class or struct NODE at org level LEVL.
Returns a list of heading strings (class heading + documented methods)."
  (let* ((name-node (treesit-node-child-by-field-name node "name"))
         (name (if name-node (treesit-node-text name-node t) "anonymous"))
         (hdn (dorgygen--heading name))
         (exis (org-find-exact-headline-in-buffer hdn))
         (body (treesit-node-child-by-field-name node "body"))
         (method-levl (concat levl "*"))
         (result (list hdn)))
    (if (not exis)
        (insert levl " " hdn "\n\n")
      (dorgygen--delete-non-user-content exis))
    (when-let ((com (dorgygen--comment-about node)))
      (insert "- " (dorgygen--format-comment com) "\n"))
    ;; data field declarations (field_declaration without function_declarator)
    (when body
      (dolist (child (treesit-node-children body t))
        (when (and (equal "field_declaration" (treesit-node-type child))
                   (not (dorgygen--find "function_declarator" child)))
          (dorgygen--cpp-field child))))
    (insert "\n")
    ;; method declarations (field_declaration with function_declarator in
    ;; tree-sitter-cpp) and inline definitions (function_definition)
    (when body
      (dolist (child (treesit-node-children body t))
        (when (or (equal "function_definition" (treesit-node-type child))
                  (and (equal "field_declaration" (treesit-node-type child))
                       (dorgygen--find "function_declarator" child)))
          (when-let ((mhdn (dorgygen--c-declaration child method-levl)))
            (push mhdn result)))))
    result))

;;; Registration

(dorgygen-add-language 'cpp
  :extensions '("hpp" "cpp" "cc" "cxx" "hh")
  :comments '("^//[ \t]?" "^/\\*[ \t]?" "[ \t]*\\*/$")
  :file-level '(("type_definition" . dorgygen--c-type_definition))
  :subheading '(("declaration"         . dorgygen--c-declaration)
                ("function_definition" . dorgygen--c-declaration)
                ("class_specifier"     . dorgygen--cpp-class)
                ("struct_specifier"    . dorgygen--cpp-class)))

(provide 'dorgygen-cpp)

;;; dorgygen-cpp.el ends here
