;;; dorgygen.el --- Source code documentation in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stefano Ghirlanda

;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (org "9.3"))
;; URL: https://github.com/drghirlanda/dorgygen
;; Keywords: development, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; dorgygen pulls source code documentation into org-mode
;; documents.  Source code documentation is embedded in comments with
;; no special markup.  The org-document can contain additional
;; documentation.

;;; Code:

(require 'org)
(require 'treesit)

(defun dorgygen--find (type node)
  "Find 1st-level children of type TYPE in the treesit tree rooted at NODE."
  (let (found)
    (dolist (child (treesit-node-children node))
      (when (equal type (treesit-node-type child))
	(push child found)))
    found))

(defun dorgygen--delete-non-user-content ()
  "Delete non-user content within current heading.
This is all content from below the headline to the end of the
first list.  Positions the point where new non-user content should
be placed."
  (when-let* ((bound (save-excursion (org-end-of-subtree)))
	      (beg (org-list-search-forward ".+" bound t)))
    (beginning-of-line)
    (delete-region
     (point)
     (org-list-get-bottom-point (org-list-struct)))))

(defun dorgygen--cleanup-comment (comm lang)
  "Remove from COMM comment markers from LANG (a symbol)."
  (cond
   ((equal lang 'c)
    (dolist (pfx '("// " "/* "))
      (setq comm (string-remove-prefix pfx comm)))
    (setq comm (string-remove-suffix " */" comm))))
  comm)


(defun dorgygen--comment-about (this &rest after)
  "Find a comment about THIS, which is a treesit-node.
If AFTER is nil, look before THIS, if non-nil, look after THIS."
  (let (sibl comm)
    (if after
	(setq sibl (treesit-node-next-sibling this t))
      (setq sibl (treesit-node-prev-sibling this t)))
    (when (and sibl
	       (equal "comment" (treesit-node-type sibl)))
      (setq comm (dorgygen--cleanup-comment
		  (treesit-node-text sibl t)
		  (treesit-node-language sibl)))
      (setq comm (concat (upcase (substring comm 0 1)) (substring comm 1)))
      ;; add full stop if missing
      (if (string-match-p "\\.$" comm)
	  comm
	(concat comm ".")))))

(defun dorgygen--not-comment (node)
  "Return t if NODE is a comment, nil otherwise."
  (not (equal "comment" (treesit-node-type node))))

(defun dorgygen--function (ndec levl)
  "Document function declaration NDEC at `org-mode' level LEVL."
  (let (exis  ; existing doc for this function
	fdec  ; function declarator
	fret  ; return type
	rcom  ; return type comment
	fnam  ; function name
	fpar) ; parameter list
    (setq fdec (car (dorgygen--find "function_declarator" ndec)))
    (when (treesit-node-p fdec)
      (setq fret (treesit-node-child ndec 0 t)
	    rcom (dorgygen--comment-about fret t)
	    fnam (treesit-node-text (treesit-node-child fdec 0 t))
	    fpar (treesit-node-child fdec 1 t))
      (setq exis (org-find-exact-headline-in-buffer fnam))
      ;; if func has no docs insert heading, else go to heading and
      ;; delete non-user comment found there
      (if (not exis)
	  (insert (format "%s %s\n\n" levl fnam))
	(goto-char exis)
	(forward-line)
	;; normalize # of \n
	(if (re-search-forward "\n+") (replace-match "\n"))
	(dorgygen--delete-non-user-content))
      ;; add documentation comment
      (let ((com (dorgygen--comment-about ndec)))
	(when com (insert "- " (dorgygen--comment-about ndec) "\n")))
      ;; add arguments and comments
      (dolist (par (treesit-filter-child fpar 'dorgygen--not-comment t))
	(insert (format "- In: ~%s~. %s\n"
			(treesit-node-text par t)
			(dorgygen--comment-about par t))))
      ;; add return type
      (insert (format "- Out: ~%s~. %s\n"
		      (treesit-node-text fret t) rcom))
      ;; normalize # of \n
      (if (re-search-forward "\n+") (replace-match "\n"))
      ;; return function name
      fnam)))

(defun dorgygen--language (file)
  "Return programming language of FILE, or nil."
  (let ((lan (org-entry-get (point) "DORGYGEN_LAN"))
	(ext (file-name-extension file)))
    (if lan
	lan
      (cond ((member ext '("h" "c")) 'c)))))

(defun dorgygen ()
  "Pull documentation from source code files into an `org-mode' document."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "Not an org-mode buffer")
    (save-excursion
      (let ((dcs '())  ; file-level docs added to buffer
	    exs  ; location of existing doc
	    buf  ; file buffer
	    par  ; file parser
	    rtn  ; parser's root node
	    lan  ; file language
	    rex  ; regexp of source files to document
	    dir  ; source directory
	    lvl) ; org level of file documentation headings
	;; find DORGYGEN_REX ascending the heading hierarcy, or abort
	(while (and
		(not (setq rex (org-entry-get (point) "DORGYGEN_REX")))
		(org-current-level))
	  (org-up-heading-safe))
	(if (not rex)
	    (message "No DORGYGEN_REX property found")
	  (setq lvl (make-string (1+ (org-current-level)) ?*))
	  (setq dir (or (file-name-directory rex) ".")
		rex (file-name-nondirectory rex))
	  ;; loop through all source files
	  (dolist (fil (directory-files dir nil rex))
	    (setq exs (org-find-exact-headline-in-buffer fil))
	    ;; if file has no docs insert heading, else go to heading
	    (if (not exs)
		(insert (concat lvl " " (file-name-nondirectory fil) "\n\n"))
	      (goto-char exs)
	      (forward-line))
	    (push fil dcs)
	    ;; determine programming language, or insert warning and move on
	    (if (not (setq lan (dorgygen--language fil)))
		(insert "Unknown programming language\n\n")
	      (setq buf (find-file-noselect fil))
	      (unless buf (error "Cannot open file %s" fil))
	      (setq par (treesit-parser-create lan buf))
	      (unless par (error "Cannot parse file %s" fil))
	      (setq rtn (treesit-parser-root-node par))
	      ;; insert function docs: 1) pass "declaration"
	      ;; statements to dorgygen--function; 2) remove returned
	      ;; function's name from running alist.
	      (dolist (ndec (dorgygen--find "declaration" rtn))
		(when-let ((fnam (dorgygen--function ndec (concat lvl "*"))))
		  (push fnam dcs)))
	      ;; cleanup
	      (treesit-parser-delete par)
	      (kill-buffer buf) ; FIX kill only if WE opened the file
              ;; mark headings still in dcs not found in source files.
	      (goto-char (org-find-exact-headline-in-buffer fil))
	      (org-map-entries (lambda () (dorgygen--update-notfound dcs)) t 'tree))))))))

(defun dorgygen--update-notfound (docs)
  "Update the notfound header tags for the current header.
DOCS is a list of current documentation headers."
  (let* ((tags1  (delete "notfound" (org-get-tags)))
	 (found (member (org-get-heading t t t t) docs))
	 (tags2 (if found tags1 (append '("notfound") tags1))))
    (org-set-tags tags2)))
      
(provide 'dorgygen)

;;; dorgygen.el ends here
