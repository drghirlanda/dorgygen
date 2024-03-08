;;; dorgygen.el --- Source code documentation in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Stefano Ghirlanda

;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (org "9.3"))
;; URL: https://github.com/drghirlanda/dorgygen
;; Keywords: tools, c, wp

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

(defgroup dorgygen nil
  "Customizations for dorgygen."
  :group 'programming)

(defcustom dorgygen-attr-list ""
  "String prepended to dorgygen list, such as #attr_latex: ..."
  :type 'string
  :group 'dorgygen)

(defun dorgygen--find (type node)
  "Find 1st-level children of type TYPE in the treesit tree rooted at NODE."
  (let (found found-child)
    (dolist (child (treesit-node-children node))
      (when (equal type (treesit-node-type child))
 	(push child found))
      (setq found-child (dorgygen--find type child))
      (when found-child
	(setq found (append (reverse found-child) found))))
    (reverse found))) ; reverse preserves file order

(defun dorgygen--delete-non-user-content ()
  "Delete non-user content within current heading.
This is all content from below the headline to the end of the
first list.  Positions the point where new non-user content should
be placed."
  (let* ((eos (save-excursion (org-end-of-subtree)))
	 (nvh (save-excursion (org-next-visible-heading 1) (point)))
	 (bnd (min eos nvh))
	 (beg (save-excursion (org-list-search-forward ".+" bnd t))))
    (when beg
      (goto-char beg)
      (beginning-of-line)
      (delete-region
       (point)
       (org-list-get-bottom-point (org-list-struct))))))

(defun dorgygen--cleanup-comment (comm lang)
  "Remove from COMM comment markers from language LANG (a symbol)."
  (when-let (delim
	     (cond ((member lang '(c cpp js))
		    '("^//\s*" "^/\\*\s*" "\s*\\*/$"))
		   ((member lang '(py sh))
		    '("^#\\s*"))))
    (save-match-data
      (dolist (d delim)
	(when (string-match d comm)
	  (setq comm (replace-match "" t t comm))))))
  comm)

(defun dorgygen--comment-about (this &rest after)
  "Find a comment about THIS, which is a treesit-node.
If AFTER is nil, look before THIS, if non-nil, look after THIS."
  (let (sibl comm)
    (if after
	(setq sibl (treesit-node-next-sibling this t))
      (setq sibl (treesit-node-prev-sibling this t)))
    (if (and
	 sibl
	 (equal "comment" (treesit-node-type sibl)))
	(progn
	  (setq comm (dorgygen--cleanup-comment
		      (treesit-node-text sibl t)
		      (treesit-node-language sibl)))
	  (setq comm (concat (upcase (substring comm 0 1))
			     (substring comm 1)))
	  ;; add full stop if missing
	  (if (string-match-p "\\.$" comm)
	      comm
	    (concat comm ".")))
      "")))

(defun dorgygen--not-comment (node)
  "Return t if NODE is a comment, nil otherwise."
  (not (equal "comment" (treesit-node-type node))))

(defun dorgygen--normalize-newlines ()
  "Replace multiple newlines with one.
Searches forward from point for `\n+' and replaces it with `\n'."
  (let ((bound (+ 2 (save-excursion (org-end-of-subtree)))))
    (while (re-search-forward "\n\\{2,\\}" bound t)
      (replace-match "\n\n"))))

(defun dorgygen--typedef (tdef)
  "Document typedef declaration TDEF."
  (let ((def (string-replace ";" "" (treesit-node-text tdef)))
	(com (dorgygen--comment-about tdef)))
    (insert (format "- ~%s~. %s\n" def com))
    def))

(defun dorgygen--function (ndec levl)
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
      (if dorgygen-attr-list (insert dorgygen-attr-list "\n"))
      ;; add documentation comment
      (let ((com (dorgygen--comment-about ndec)))
	(when com (insert "- " (dorgygen--comment-about ndec) "\n")))
      ;; add arguments and comments
      (dolist (par (treesit-filter-child fpar 'dorgygen--not-comment t))
	(insert (format "- In: ~%s~. %s\n"
			(treesit-node-text par t)
			(dorgygen--comment-about par t))))
      ;; add return type and 2 \n to terminate list
      (insert (format "- Out: ~%s%s~. %s\n\n"
		      (treesit-node-text fret t) fpnt rcom))
      ;; return heading
      fhdn)))

(defun dorgygen--language (file)
  "Return programming language of FILE, or nil."
  (let ((lan (org-entry-get (point) "DORG_LAN"))
	(ext (file-name-extension file)))
    (if lan
	lan
      (cond ((member ext '("h" "c")) 'c)))))

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
	    exs  ; location of existing doc
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
	;; loop through source files
	(dolist (fil (directory-files-recursively dir rex))
	  (unless (setq lan (dorgygen--language fil))
	    (error "dorgygen: Language %s unknown" lan))
	  (unless (treesit-language-available-p lan)
	    (error "dorgygen: Language %s not available in tree-sitter" lan))
	  ;; kll used to kill fil's buffer later, if we did not open it
	  (when (not (get-file-buffer fil))
	    (setq kll t))
	  (setq buf (find-file-noselect fil))
	  ;; ensure <lan>-ts-mode in buf
	  (with-current-buffer buf
	    (eval (car (read-from-string
			(concat "(" (symbol-name lan) "-ts-mode)")))))
	  (unless (treesit-parser-list buf)
	    (error "dorgygen: Cannot create parser for %s" fil))
	  (setq par (car (treesit-parser-list buf))
		rtn (treesit-parser-root-node par)
		hdn (dorgygen--heading fil)
		exs (org-find-exact-headline-in-buffer hdn))
	  ;; if file has no docs insert heading, else go to heading
	  (if (not exs)
	      (insert lvl " " hdn "\n\n")
	    (goto-char exs)
	    (forward-line)
	    (dorgygen--delete-non-user-content))
	  (push hdn dcs) ; add to found docs
	  ;; insert typedef docs
	  (when-let ((typedefs (dorgygen--find "type_definition" rtn)))
	    (insert dorgygen-attr-list "\n")
	    (dolist (td typedefs) (dorgygen--typedef td))
	    (insert "\n"))
	  ;; insert function docs
	  (dolist (ndec (dorgygen--find "declaration" rtn))
	    (when-let ((fnam (dorgygen--function ndec (concat lvl "*"))))
	      (push fnam dcs))) ; add to found docs
	  ;; cleanup
	  (when kll (kill-buffer buf)) ; kill buf iff we created it
          ;; mark headings still in dcs as not found
	  (goto-char (org-find-exact-headline-in-buffer hdn))
	  (org-map-entries (lambda () (dorgygen--update-notfound dcs)) t 'tree)
	  ;; get rid of excessive newlines
	  (dorgygen--normalize-newlines))))))

(defun dorgygen--update-notfound (docs)
  "Update the notfound header tags for the current header.
DOCS is a list of current documentation headers."
  (let* ((tags1  (delete "notfound" (org-get-tags)))
	 (found (member (org-get-heading t t t t) docs))
	 (tags2 (if found tags1 (append '("notfound") tags1))))
    (org-set-tags tags2)))
      
(provide 'dorgygen)

;;; dorgygen.el ends here
