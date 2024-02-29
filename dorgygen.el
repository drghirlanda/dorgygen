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
;; documents. Source code documentation is embedded in comments with
;; no special markup. The org-document can contain additional
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
first list. Positions the point where new non-user content should
be placed."
  (org-back-to-heading)
  (forward-line)
  (let ((beg   (point))
	(bound (save-excursion
		 (unless (org-goto-first-child)
		   (org-end-of-subtree))
		 (point))))
    ;; look for 1st end-of-list (- line followed by a blank line)
    (when (re-search-forward "^\s*-\s+.*\n\n" bound t)
      (backward-char) ; leave last \n
      (delete-region beg (point)))))

(defun dorgygen--comment-about (this &rest after)
  "Find a comment about THIS, which is a treesit-node.
If AFTER is nil, look before THIS, if non-nil, look after THIS."
  (let ((sibl (if after
		  (treesit-node-next-sibling this t)
		(treesit-node-prev-sibling this t)))
	comm)
    (when (and sibl
	       (equal "comment" (treesit-node-type sibl)))
      (setq comm (string-replace
		 "// "
		 ""
		 (treesit-node-text sibl t)))
      (concat (upcase (substring comm 0 1)) (substring comm 1)))))


(defun dorgygen--not-comment (node)
  "Return t if NODE is a comment, nil otherwise."
  (not (equal "comment" (treesit-node-type node))))

(defun dorgygen--function (ndec levl)
  "Document function declaration NDEC at `org-mode' level LEVL."
  (let (docs  ; existing function doc headings
	exis  ; existing doc for this function
	fdec  ; function declarator
	fret  ; return type
	rcom  ; return type comment
	fnam  ; function name
	fpar) ; parameter list
    (setq fdec (car (dorgygen--find "function_declarator" ndec)))
    (when (treesit-node-p fdec)
      (setq fret (treesit-node-child ndec 0 t)
	    rcom (dorgygen--comment-about fret t)
	    fnam (treesit-node-child fdec 0 t)
	    fpar (treesit-node-child fdec 1 t))
      ;; alist of (heading . point) values of existing function docs
      (setq docs (org-map-entries 'dorgygen--heading-point t 'tree))
      (setq exis (assoc (treesit-node-text fnam) docs))
      ;; if function has no docs, insert heading; else cleanup heading
      (if (not exis)
	  (insert (concat levl " " (treesit-node-text fnam) "\n"))
	(goto-char (cdr exis))
	(dorgygen--delete-non-user-content))
      ;; documentation comment
      (insert "\n- " (dorgygen--comment-about ndec))
      ;; iterate over arguments and comments:
      (dolist (par (treesit-filter-child fpar 'dorgygen--not-comment t))
	(insert (concat
		 "\n- In: ~"
		 (treesit-node-text par t) "~. "
		 (dorgygen--comment-about par t))))
      ;; append return type:
      (insert (concat
	       "\n- Out: ~"
	       (treesit-node-text fret t)
	       "~. "
	       rcom))
      ;; finish list
      (insert "\n"))))

(defun dorgygen--functions (root levl)
  "Document functions under `treesit' node ROOT at `org-mode' level LEVL."
  (dolist (ndec (dorgygen--find "declaration" root))
    (dorgygen--function ndec levl)))

(defun dorgygen--language (file)
  "Get (or guess) programming language of FILE."
  (let ((lan (org-entry-get (point) "DORGYGEN_LAN")))
    (if lan
	lan
      (save-match-data
	(if (string-match "\\.\\(.+\\)$" file)
	    (match-string 1 file)
	  nil)))))

(defun dorgygen--heading-point ()
  "Return an alist of (<heading title> . (point)) pairs."
  (cons (substring-no-properties (org-get-heading)) (point)))

(defun dorgygen ()
  "Pull documentation from source code files into an `org-mode' document."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "Not an org-mode buffer")
    (save-excursion
      (let (dcs  ; file-level docs already in buffer
	    exs  ; location of existing file-level doc
	    buf  ; file buffer
	    par  ; file parser
	    lan  ; file language
	    dir  ; directory part of ORGDOC_REX
	    rex  ; regexp of source files to document
	    lvl) ; org level of file documentation headings
	;; look for ORGDOC_REX here and upward the heading hierarcy.
	;; abort if not found
	(while (and (not (setq rex (org-entry-get (point) "DORGYGEN_REX")))
		    (org-current-level))
	  (org-up-heading-safe))
	(if (not rex)
	    (message "No DORGYGEN_REX property found")
	  (setq lvl (make-string (1+ (org-current-level)) ?*))
	  (setq dir (or (file-name-directory rex) ".")
		rex (file-name-nondirectory rex))
	  ;; alist of (heading . point) values of existing docs
	  (setq dcs (org-map-entries 'dorgygen--heading-point t 'tree))
	  (dolist (fil (directory-files dir nil rex))
	    (setq exs (assoc fil dcs))
	    ;; if file has no docs, insert heading; else cleanup heading
	    (if (not exs)
		(insert (concat lvl " " (file-name-nondirectory fil) "\n"))
	      (goto-char (cdr exs))
	      (dorgygen--delete-non-user-content))
	    ;; determine programming language, or insert warning and move on
	    (setq lan (dorgygen--language fil))
	    (if (not lan)
		(insert "\nUnknown programming language for " fnam)
	      (setq buf (find-file-noselect fil)
		    par (treesit-parser-create 'c buf))
	      ;; move to just before 1st function, if present
	      (when (org-goto-first-child)
		(forward-line -1))
	      (dorgygen--functions (treesit-parser-root-node par)
				   (1+ (org-current-level)))
	      (treesit-parser-delete par)
	      (kill-buffer buf))))))))

(provide 'dorgygen)

;;; dorgygen.el ends here
