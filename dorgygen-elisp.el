;;; dorgygen-elisp.el --- Emacs Lisp support for dorgygen  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stefano Ghirlanda

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Emacs Lisp language support for dorgygen.
;;
;; Supported constructs:
;;   - Top-level functions (defun) and macros (defmacro): docstring and arglist
;;   - Top-level variables: defvar and defconst with docstrings
;;
;; Load with (require 'dorgygen-elisp) or (dorgygen-use-languages 'elisp).

;;; Code:

(require 'dorgygen)

;;; Mode activation

(defun dorgygen--elisp-activate ()
  "Enable a tree-sitter parser for the current Emacs Lisp buffer."
  (unless (derived-mode-p 'emacs-lisp-mode)
    (emacs-lisp-mode))
  (treesit-parser-create 'elisp))

;;; Helpers

(defun dorgygen--elisp-clean-string (node)
  "Extract the first line of a docstring from string NODE."
  (let* ((raw (treesit-node-text node t))
         (text (if (string-prefix-p "\"" raw)
                   (substring raw 1 (1- (length raw)))
                 raw))
         (text (replace-regexp-in-string "\\\\\"" "\"" text))
         (text (car (split-string text "\n")))
         (text (string-trim text)))
    (unless (string-empty-p text) text)))

(defun dorgygen--elisp-params (arglist-node)
  "Return a list of (LABEL . NAME) pairs from ARGLIST-NODE.
LABEL is \"param\", \"optional\", or \"rest\"."
  (let ((label "param")
        result)
    (dolist (child (treesit-node-children arglist-node t))
      (let ((text (treesit-node-text child t)))
        (cond
         ((equal text "&optional") (setq label "optional"))
         ((equal text "&rest")     (setq label "rest"))
         (t (push (cons label text) result)))))
    (nreverse result)))

;;; Handlers

(defun dorgygen--elisp-defun (node levl)
  "Document defun or defmacro NODE at org level LEVL."
  (let* ((name-node (treesit-node-child node 2))
         (name (when name-node (treesit-node-text name-node t)))
         (hdn (dorgygen--heading name))
         (exis (org-find-exact-headline-in-buffer hdn))
         (arglist-node (treesit-node-child node 3))
         (doc-node (treesit-node-child node 4))
         (doc (when (and doc-node (equal "string" (treesit-node-type doc-node)))
                (dorgygen--elisp-clean-string doc-node))))
    (when name
      (if (not exis)
          (insert levl " " hdn "\n\n")
        (dorgygen--delete-non-user-content exis))
      (when doc
        (insert "- " doc "\n"))
      (when arglist-node
        (dolist (param (dorgygen--elisp-params arglist-node))
          (insert (format "- %s ~%s~\n" (car param) (cdr param)))))
      (insert "\n")
      hdn)))

(defun dorgygen--elisp-variable (node)
  "Document defvar or defconst NODE as a file-level item."
  (let* ((kw-node (treesit-node-child node 1))
         (kw (when kw-node (treesit-node-type kw-node))))
    (when (member kw '("defvar" "defconst"))
      (let* ((name-node (treesit-node-child node 2))
             (name (when name-node (treesit-node-text name-node t)))
             (doc-node (treesit-node-child node 4))
             (doc (when (and doc-node (equal "string" (treesit-node-type doc-node)))
                    (dorgygen--elisp-clean-string doc-node))))
        (when name
          (insert (format "- ~%s~%s\n" name
                          (if doc (concat " :: " doc) ""))))))))

;;; Registration

(dorgygen-add-language 'elisp
  :extensions '("el")
  :mode #'dorgygen--elisp-activate
  :comments '("^;+[ \t]*")
  :file-level '(("special_form" . dorgygen--elisp-variable))
  :subheading '(("function_definition" . dorgygen--elisp-defun)
                ("macro_definition"    . dorgygen--elisp-defun)))

(provide 'dorgygen-elisp)

;;; dorgygen-elisp.el ends here
