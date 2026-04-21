;;; dorgygen-c.el --- C support for dorgygen  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stefano Ghirlanda

;; This file is not part of GNU Emacs.

;;; Commentary:

;; C language support for dorgygen.  Documentation is extracted from
;; // and /* */ comments adjacent to declarations.
;;
;; Supported constructs:
;;   - Function declarations (parameters, return type, comments)
;;   - Typedef declarations
;;
;; Load with (require 'dorgygen-c).

;;; Code:

(require 'dorgygen)

;;; Handlers

(defun dorgygen--c-type_definition (tdef)
  "Document typedef declaration TDEF."
  (let ((def (string-replace ";" "" (treesit-node-text tdef)))
	(com (dorgygen--comment-about tdef)))
    (insert (format "- ~%s~.%s\n" def (if com (concat " " (dorgygen--format-comment com)) "")))
    def))

(defun dorgygen--c-declaration (ndec levl)
  "Document function declaration NDEC at `org-mode' level LEVL."
  (let (exis  ; existing doc for this function
	fdec  ; function declarator
	fret  ; return type
	rcom  ; return type comment
	fnam  ; function name
	fpar  ; parameter list
	fhdn  ; org heading for this function
	prnt  ; parent node
	fpnt) ; "*" if return type is a pointer, else ""
    (setq fdec (car (dorgygen--find "function_declarator" ndec)))
    (when (treesit-node-p fdec)
      (setq fret (treesit-node-child ndec 0 t)
	    rcom (dorgygen--comment-about fret t)
	    fnam (treesit-node-text (treesit-node-child fdec 0 t))
	    fhdn (dorgygen--heading fnam)
	    fpar (treesit-node-child fdec 1 t)
	    prnt (treesit-node-parent fdec))
      (if (not (equal "pointer_declarator" (treesit-node-type prnt)))
	  (setq fpnt "")
	(setq fpnt " *"
	      rcom (dorgygen--comment-about fdec)))
      (setq exis (org-find-exact-headline-in-buffer fhdn))
      (if (not exis)
	  (insert levl " " fhdn "\n\n")
	(goto-char exis)
	(forward-line)
	(dorgygen--delete-non-user-content))
      (unless (string-empty-p dorgygen-attr-list)
	(insert dorgygen-attr-list "\n"))
      (let ((com (dorgygen--comment-about ndec)))
	(when com (insert "- " (dorgygen--format-comment com) "\n")))
      (dolist (par (treesit-filter-child fpar 'dorgygen--not-comment t))
	(let ((com (dorgygen--comment-about par t)))
	  (insert (format "- In: ~%s~.%s\n"
			  (treesit-node-text par t)
			  (if com (concat " " (dorgygen--format-comment com)) "")))))
      (insert (format "- Out: ~%s%s~.%s\n\n"
		      (treesit-node-text fret t) fpnt
		      (if rcom (concat " " (dorgygen--format-comment rcom)) "")))
      fhdn)))

;;; Registration

(dorgygen-add-language 'c
  :extensions '("h" "c")
  :comments '("^//[ \t]?" "^/\\*[ \t]?" "[ \t]*\\*/$")
  :file-level '(("type_definition" . dorgygen--c-type_definition))
  :subheading '(("declaration" . dorgygen--c-declaration)))

(provide 'dorgygen-c)

;;; dorgygen-c.el ends here
