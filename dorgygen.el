;;; dorgygen.el --- Source code documentation in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stefano Ghirlanda

;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/drghirlanda/dorgygen
;; Keywords: tools, wp

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

;; Dorgygen pulls documentation from source files into org-mode
;; documents using tree-sitter.  Source code documentation is
;; embedded in comments with no special markup.  The org document
;; can contain additional documentation.
;;
;; Usage: Add a DORG_REX property to an org heading, then M-x dorgygen.
;; New languages can be added with `dorgygen-add-language'.
;; See https://github.com/drghirlanda/dorgygen for full documentation.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'treesit)

(defgroup dorgygen nil
  "Customizations for dorgygen."
  :group 'programming)

(defcustom dorgygen-attr-list ""
  "String prepended to dorgygen list, like \"#attr_latex: ...\"."
  :type 'string
  :group 'dorgygen)
  
(defun dorgygen--find (type node)
  "Find all descendants of type TYPE in the treesit tree rooted at NODE."
  (let (found found-child)
    (dolist (child (treesit-node-children node))
      (when (equal type (treesit-node-type child))
 	(push child found))
      (setq found-child (dorgygen--find type child))
      (when found-child
	(setq found (append (reverse found-child) found))))
    (reverse found))) ; reverse preserves file order

(defun dorgygen--delete-non-user-content ()
  "Delete auto-generated content within current heading.
Point should be on the line after the heading.  Deletes from the
first list item to the next heading or end of subtree, preserving
any user content between the heading and the list."
  (let* ((eos (save-excursion (org-end-of-subtree)))
	 (nvh (save-excursion (org-next-visible-heading 1) (point)))
	 (bnd (min eos nvh))
	 (lst (save-excursion
		(when (re-search-forward "^- " bnd t)
		  (line-beginning-position)))))
    (if lst
	(progn
	  (goto-char lst)
	  (delete-region lst bnd)
	  (insert "\n"))
      (goto-char bnd)
      (insert "\n"))))

(defvar dorgygen--language-alist nil
  "Alist mapping languages to their dorgygen configuration.
Each entry is (LANGUAGE . PLIST) where PLIST has keys:
  :extensions  - list of file extensions (e.g., (\"h\" \"c\"))
  :comments    - list of regexps to strip from comment text
  :file-level  - alist of (TREESIT-TYPE . HANDLER-FN) for list items
  :subheading  - alist of (TREESIT-TYPE . HANDLER-FN) for subheadings")

(defun dorgygen-add-language (language &rest props)
  "Register LANGUAGE for dorgygen.
PROPS is a plist with keys :extensions, :comments, :file-level, :subheading.
See `dorgygen--language-alist' for details."
  (setf (alist-get language dorgygen--language-alist) props))

(defun dorgygen--cleanup-comment (node)
  "Get comment text from NODE, removing comment markers.

This function removes all start and end comment markers.  For
example, if a string starts with two comment markers, they will
both be deleted."
  (when-let* ((comm (treesit-node-text node t))
	      (lang (treesit-node-language node))
	      (dels (plist-get (cdr (assoc lang dorgygen--language-alist))
			       :comments)))
    (save-match-data
      (dolist (d dels)
	(when (string-match d comm)
	  (setq comm (replace-match "" t t comm)))))
    ;; add full stop if missing
    (if (string-match-p "\\.$" comm)
	comm
      (concat comm "."))))

(defun dorgygen--comment-about (this &rest after)
  "Find a comment about THIS (a treesit node).
If AFTER is nil, look before THIS, if non-nil, look after THIS."
  (let ((sibl (if after
		  (treesit-node-next-sibling this t)
		(treesit-node-prev-sibling this t))))
    (when (and sibl (equal "comment" (treesit-node-type sibl)))
      (let* ((comm (dorgygen--cleanup-comment sibl))
	     ;; capitalize 1st letter
	     (comm (concat (upcase (substring comm 0 1))
			   (substring comm 1)))
	     ;; look for more comment lines before or after
	     (more (if after
			(dorgygen--comment-about sibl t)
		      (dorgygen--comment-about sibl))))
	(if more
	    (if after
		(concat comm " " more)
	      (concat more " " comm))
	  comm)))))

(defun dorgygen--not-comment (node)
  "Return t if NODE is a comment, nil otherwise."
  (not (equal "comment" (treesit-node-type node))))

(defun dorgygen--normalize-newlines ()
  "Replace 3+ consecutive newlines with two, from point to end of buffer.
Also remove trailing whitespace from lines."
  (while (re-search-forward "[ \t]+$" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\n\\{3,\\}" nil t)
    (replace-match "\n\n")))

(defun dorgygen--c-type_definition (tdef)
  "Document typedef declaration TDEF."
  (let ((def (string-replace ";" "" (treesit-node-text tdef)))
	(com (dorgygen--comment-about tdef)))
    (insert (format "- ~%s~.%s\n" def (if com (concat " " com) "")))
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
      ;; if func has no docs insert heading, else go to heading and
      ;; delete non-user comment found there
      (if (not exis)
	  (insert levl " " fhdn "\n\n")
	(goto-char exis)
	(forward-line)
	(dorgygen--delete-non-user-content))
      ;; add customization line
      (unless (string-empty-p dorgygen-attr-list)
	(insert dorgygen-attr-list "\n"))
      ;; add documentation comment
      (let ((com (dorgygen--comment-about ndec)))
	(when com (insert "- " com "\n")))
      ;; add arguments and comments
      (dolist (par (treesit-filter-child fpar 'dorgygen--not-comment t))
	(let ((com (dorgygen--comment-about par t)))
	  (insert (format "- In: ~%s~.%s\n"
			  (treesit-node-text par t)
			  (if com (concat " " com) "")))))
      ;; add return type and 2 \n to terminate list
      (insert (format "- Out: ~%s%s~.%s\n\n"
		      (treesit-node-text fret t) fpnt
		      (if rcom (concat " " rcom) "")))
      ;; return heading
      fhdn)))

(defun dorgygen--language (file)
  "Return programming language of FILE, or nil."
  (let ((lan (org-entry-get (point) "DORG_LAN"))
	(ext (file-name-extension file)))
    (if lan
	(intern lan)
      (cl-loop for (lang . props) in dorgygen--language-alist
	       when (member ext (plist-get props :extensions))
	       return lang))))

(defun dorgygen--heading (name)
  "Remove leading ./ from NAME and surround with ~."
  (concat "~" (string-remove-prefix "./" name) "~"))

(defun dorgygen ()
  "Pull documentation from source code files into an `org-mode' document."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "Not an org-mode buffer")
    (save-excursion
      (let ((dcs '())  ; file-level docs added to buffer
	    buf  ; file buffer
	    par  ; file parser
	    rtn  ; parser's root node
	    lan  ; file language
	    rex  ; regexp of source files to document
	    dir  ; source directory
	    kll  ; kill buffer only if we opened it
	    hdn  ; org heading for current documentation entry
	    lvl) ; org level of file documentation headings
	;; find DORG_REX ascending the heading hierarcy, or abort
	(while (and
		(not (setq rex (org-entry-get (point) "DORG_REX")))
		(org-current-level))
	  (org-up-heading-safe))
	(unless rex
	  (error "dorgygen: Heading has no DORG_REX property"))
	(setq lvl (make-string (1+ (org-current-level)) ?*))
	(setq dir (or (file-name-directory rex) "./")
	      rex (file-name-nondirectory rex))
	;; move past heading and properties drawer
	(org-end-of-meta-data t)
	;; loop through source files
	(dolist (fil (directory-files-recursively dir rex))
	  (unless (setq lan (dorgygen--language fil))
	    (error "dorgygen: Language %s unknown" lan))
	  (unless (treesit-language-available-p lan)
	    (error "dorgygen: Language %s not available in tree-sitter" lan))
	  ;; if fil has no buffer, kill it when we're done with it
	  (setq kll (not (get-file-buffer fil)))
	  (setq buf (find-file-noselect fil))
	  ;; ensure <lan>-ts-mode in buf
	  (with-current-buffer buf
	    (funcall (intern (concat (symbol-name lan) "-ts-mode"))))
	  (unless (treesit-parser-list buf)
	    (error "dorgygen: Cannot create parser for %s" fil))
	  (setq par (car (treesit-parser-list buf))
		rtn (treesit-parser-root-node par)
		hdn (dorgygen--heading fil))
	  ;; if file has no docs insert heading, else go to heading
	  (let ((exs (org-find-exact-headline-in-buffer hdn)))
	    (if (not exs)
		(insert lvl " " hdn "\n\n")
	      (goto-char exs)
	      (forward-line)
	      (dorgygen--delete-non-user-content)))
	  ;; add heading to list of docs in file
	  (push hdn dcs)
	  ;; insert file-level docs (list items under file heading)
	  (let ((cfg (cdr (assoc lan dorgygen--language-alist))))
	    (when-let ((fl-entries (plist-get cfg :file-level)))
	      (let (has-items)
		(dolist (entry fl-entries)
		  (when-let ((nodes (dorgygen--find (car entry) rtn)))
		    (unless has-items
		      (unless (string-empty-p dorgygen-attr-list)
			(insert dorgygen-attr-list "\n"))
		      (setq has-items t))
		    (dolist (node nodes)
		      (funcall (cdr entry) node))))
		(when has-items (insert "\n"))))
	    ;; insert subheading docs
	    (dolist (entry (plist-get cfg :subheading))
	      (dolist (node (dorgygen--find (car entry) rtn))
		(let ((result (funcall (cdr entry) node (concat lvl "*"))))
		  (cond
		   ((stringp result) (push result dcs))
		   ((listp result) (setq dcs (append result dcs))))))))  ; add to found docs
	  ;; cleanup
	  (when kll (kill-buffer buf)) ; kill buf iff we created it
          ;; mark headings still in dcs as not found
	  (goto-char (org-find-exact-headline-in-buffer hdn))
	  (org-map-entries (lambda () (dorgygen--update-notfound dcs)) t 'tree))
	;; get rid of excessive newlines across all generated docs
	(goto-char (point-min))
	(dorgygen--normalize-newlines)))))

(defun dorgygen--update-notfound (docs)
  "Update the notfound header tags for the current header.
DOCS is a list of current documentation headers."
  (let* ((tags1  (delete "notfound" (org-get-tags)))
	 (found (member (org-get-heading t t t t) docs))
	 (tags2 (if found tags1 (append '("notfound") tags1))))
    (org-set-tags tags2)))
      
;; Register built-in languages

(dorgygen-add-language 'c
  :extensions '("h" "c")
  :comments '("^//[ \t]*" "^/\\*[ \t]*" "[ \t]*\\*/$")
  :file-level '(("type_definition" . dorgygen--c-type_definition))
  :subheading '(("declaration" . dorgygen--c-declaration)))

(provide 'dorgygen)

;;; dorgygen.el ends here
