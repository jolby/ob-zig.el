;;; ob-zig.el --- Org Babel functions for Zig evaluation

;; Copyright (C) Joel Boehland

;; Author: Joel Boehland
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating Zig code.

;;; Requirements:

;; Use this section to list the requirements of this language.  Most
;; languages will require that at least the language be installed on
;; the user's system, and the Emacs major mode relevant to the
;; language be installed as well.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'org-macs)
;;require modes required for your language
(require 'zig-mode)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("zig" . "zig"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:zig '())

(defconst org-babel-header-args:zig '((includes . :any)
                                      (imports . :any)
                                      (main    . :any)
                                      (flags   . :any)
                                      (cmdline . :any)
                                      (libs    . :any))
  "Zig-specific header arguments.")

(defcustom org-babel-zig-compiler "zig"
  "Command used to compile a zig source code file into an executable.
May be either a command in the path, like zig
or an absolute path name, like /usr/local/bin/zig
parameter may be used, like zig -v"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defcustom org-babel-zig-integer-type "isize"
  "Default zig integer type"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defcustom org-babel-zig-floating-point-type "f64"
  "Default zig floating-point type"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defcustom org-babel-zig-string-type "[]const u8"
  "Default zig string type"
  :group 'org-babel
  :version "24.3"
  :type 'string)

(defun org-babel-expand-body:zig (body params &optional processed-params)
  "Expand a block of zig code with org-babel according to
its header arguments."
  (let ((vars (org-babel--get-vars params))
	(colnames (cdr (assq :colname-names params)))
	(main-p (not (string= (cdr (assq :main params)) "no")))
        (imports (org-babel-read
                  (cdr (assq :imports params))
                  nil))
        (using-namespaces (org-babel-read
                           (cdr (assq :using-namespaces params))
		           nil))
	(c-includes (org-babel-read
		     (cdr (assq :c-includes params))
		     nil))
	(c-defines (org-babel-read
		    (cdr (assq :c-defines params))
		    nil)))
    (when (stringp imports)
      (setq imports (split-string imports)))
    (when (stringp using-namespaces)
      (setq using-namespaces (split-string using-namespaces)))
    (when (stringp c-includes)
      (setq c-includes (split-string c-includes)))
    (when (stringp c-defines)
      (let ((y nil)
	    (result (list t)))
	(dolist (x (split-string c-defines))
	  (if (null y)
	      (setq y x)
	    (nconc result (list (concat y " " x)))
	    (setq y nil)))
	(setq c-defines (cdr result))))
    (mapconcat 'identity
	       (list
                ;; imports
                (mapconcat
                 (lambda (inc)
                   ;; :imports '(std testing) gives us a list of
                   ;; symbols; convert those to strings.
                   (when (symbolp inc) (setq inc (symbol-name inc)))
                   (format "const %s = @import(\"%s\");" inc inc))
                 imports "\n")
		;; c-includes
		(mapconcat
		 (lambda (inc)
		   ;; :c-includes '("stdint.h" "string.h") gives us a list of
		   ;; symbols; convert those to strings.
		   (when (symbolp inc) (setq inc (symbol-name inc)))
                   (format "@cInclude(\"%s\");" inc))
		 c-includes "\n")
		;; c-defines
		(mapconcat
		 (lambda (inc) (format "@cDefine(%s);" inc))
		 (if (listp c-defines) c-defines (list c-defines)) "\n")
		;; using-namespaces
		(mapconcat
		 (lambda (inc) (format "usingnamespace %s;" inc))
		 using-namespaces
		 "\n")
		;; variables
		(mapconcat 'org-babel-zig-var-to-zig-source vars "\n")
		;; ;; tables headers utility
		(when colnames
		  (org-babel-zig-utility-header-to-zig))
		;; ;; tables headers
                (mapconcat (lambda (head)
                             (let* ((tblnm (car head))
                                    (tbl (cdr (car (let* ((el vars))
                                                     (while (not (or (equal tblnm (caar el)) (not el)))
                                                       (setq el (cdr el)))
                                                     el))))
                                    (type-descriptor (org-babel-zig-val-type-descriptor tbl)))
                               (org-babel-zig-header-to-zig head type-descriptor))) colnames "\n")
		;; body
		(if main-p
		    (org-babel-zig-ensure-main-wrap body)
		  body) "\n") "\n")))


(defun org-babel-zig-ensure-main-wrap (body)
  "Wrap BODY in a \"main\" function call if none exists."
  (if (string-match "pub fn[ \t\n\r]*main(.*)" body)
      body
    (format "pub fn main() !void {\n%s\n}\n" body)))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:zig (body params)
  "Execute a block of Zig code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Zig source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the session variable is non-nil
         ;; (session (org-babel-zig-initiate-session (nth 0 processed-params)))
         ;; variables assigned for use in the block
         (vars (nth 1 processed-params))
         (result-params (nth 2 processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (nth 3 processed-params))
         ;; expand the body with `org-babel-expand-body:zig'
         (tmp-src-file (org-babel-temp-file
			"Zig-src-" ".zig"))
	 ;; (tmp-bin-file
	 ;;  (org-babel-process-file-name
	 ;;   (org-babel-temp-file "Zig-bin-" org-babel-exeext)))
	 (cmdline (cdr (assq :cmdline params)))
	 (cmdline (if cmdline (concat " " cmdline) ""))
	 (flags (cdr (assq :flags params)))
	 (flags (mapconcat 'identity
			   (if (listp flags) flags (list flags)) " "))
	 (libs (org-babel-read
		(or (cdr (assq :libs params))
		    (org-entry-get nil "libs" t))
		nil))
	 (libs (mapconcat #'identity
			  (if (listp libs) libs (list libs))
			  " "))
         (full-body (org-babel-expand-body:zig
                     body params processed-params))
         (full-command (format "%s run %s"
                               org-babel-zig-compiler
                               (org-babel-process-file-name tmp-src-file))))
    ;; (message "BODY: %s" full-body)
    ;; (message "CMD: %s" full-command)
    (with-temp-file tmp-src-file (insert full-body))
    (let ((results (org-babel-eval full-command "")))
      ;; (message "CMD RES: %s" results)
      ;; results
      (when results
        (setq results (org-remove-indentation results))
        (org-babel-reassemble-table
         (org-babel-result-cond (cdr (assq :result-params params))
           (org-babel-read results t)
           (let ((tmp-file (org-babel-temp-file "zig-")))
             (with-temp-file tmp-file (insert results))
             (org-babel-import-elisp-from-file tmp-file)))
         (org-babel-pick-name
          (cdr (assq :colname-names params)) (cdr (assq :colnames params)))
         (org-babel-pick-name
          (cdr (assq :rowname-names params)) (cdr (assq :rownames params))))))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:zig (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )


(defun org-babel-zig-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-zig-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")))

;; helper functions
(defun org-babel-zig-format-list-val (list-vals format-data)
  (concat "{"
          (mapconcat (lambda (v) (format format-data v)) list-vals ",")
          "}"))

(defun org-babel-zig-list-val-to-zig (val type-descriptor)
  (let* ((dims (format "[%d]" (length val)))
         (zig-list-val (org-babel-zig-format-list-val val (plist-get type-descriptor :format-data))))
    (plist-put type-descriptor :dims dims)
    (plist-put type-descriptor :zig-value zig-list-val)))

(defun org-babel-zig-format-table-val (table-vals format-data inner-dims zig-type)
  (concat "{\n"
          (mapconcat (lambda (v)
                       (concat
                        "    " ;indentation
                        inner-dims
                        zig-type
                        "{"
                        (mapconcat
                         (lambda (w) (format format-data w))
                         v ",")
                        "}"))
                     table-vals
                     ",\n")
          "}"))

(defun org-babel-zig-table-val-to-zig (val type-descriptor)
  (let* ((dims (format "[%d][%d]" (length val) (length (car val))))
         (inner-dims (format "[%d]" (length (car val))))
         (format-data (plist-get type-descriptor :format-data))
         (zig-type (plist-get type-descriptor :zig-type))
         (zig-table-val (org-babel-zig-format-table-val val format-data inner-dims zig-type)))
    (plist-put type-descriptor :dims dims)
    (plist-put type-descriptor :zig-value zig-table-val)))

(defun org-babel-zig-val-to-zig (val type-descriptor)
  (let* ((format-data (plist-get type-descriptor :format-data))
        (zig-val-str (if (stringp format-data)
                         ;; (cons "" (format format-data val))
                         (format format-data val)
                       (funcall format-data val))))
        (plist-put type-descriptor :zig-value zig-val-str)))

(defun org-babel-zig-val-to-base-type (val)
  "Determine the base type of VAL which may be
`integerp' if all base values are integers
`floatp' if all base values are either floating points or integers
`stringp' otherwise."
  (cond
   ((integerp val) 'integerp)
   ((floatp val) 'floatp)
   ((or (listp val) (vectorp val))
    (let ((type nil))
      (mapc (lambda (v)
              (pcase (org-babel-zig-val-to-base-type v)
                (`stringp (setq type 'stringp))
                (`floatp
                 (when (or (not type) (eq type 'integerp))
                   (setq type 'floatp)))
                (`integerp
                 (unless type (setq type 'integerp)))))
            val)
      type))
   (t 'stringp)))

(defun org-babel-zig-val-type-descriptor (val)
  "Determine the type of VAL.

Return a plist:
(:base-type base elisp type of VAL 'stringp|'integerp|'floatp
 :zig-type formatted string of the zig type, suitable for output in source code
 :rank scalar|list|table
 :format format-data|format-function
 :dims formatted-dimensions or empty string, suitable for output in source code "
  (let* ((base-type (org-babel-zig-val-to-base-type val))
         (rank (cond
                ((and (listp val) (listp (car val))) "table")
                ((and (or (listp val) (vectorp val))) "list")
                (t "scalar")))
         (formatter
          (pcase base-type
            (`integerp "%d")
            (`floatp "%f")
            (`stringp "\"%s\"")
            (_ (error "Unknown type %S" base-type))))
         (zig-type
          (pcase base-type
            (`integerp org-babel-zig-integer-type)
            (`floatp org-babel-zig-floating-point-type)
            (`stringp org-babel-zig-string-type)
            (_ (error "Unknown type %S" base-type))))
         (type-descriptor
          `(:base-type ,base-type
            :zig-type ,zig-type
            :rank ,rank
            :format-data ,formatter)))
    ;; (message "TYPE D: %s" type-descriptor)
    type-descriptor))

(defun org-babel-zig-val-to-zig-type (val)
  "Determine the type of VAL.
Return a plist"
  (let* ((type-descriptor (org-babel-zig-val-type-descriptor val)))
    (cond
     ((string= "table" (plist-get type-descriptor :rank)) (org-babel-zig-table-val-to-zig val type-descriptor))
     ((string= "list" (plist-get type-descriptor :rank)) (org-babel-zig-list-val-to-zig val type-descriptor))
     (t (org-babel-zig-val-to-zig val type-descriptor)))))

(defun org-babel-zig-var-to-zig-source (pair)
  "Convert an elisp val into a string of zig code specifying a var
of the same value."
  ;; TODO list support
  (let ((var (car pair))
	(val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
	(setq val (string-to-char val))))
    (let* ((type-data (org-babel-zig-val-to-zig-type val))
           (var-data (plist-put type-data :var-name var)))
      ;; (message "Var data: %s" var-data)
      (cond
       ((or (string= "table" (plist-get var-data :rank)) (string= "list" (plist-get var-data :rank)))
        (format "const %s = %s%s%s;"
                        (plist-get var-data :var-name)
                        (plist-get var-data :dims)
                        (plist-get var-data :zig-type)
                        (plist-get var-data :zig-value)))
       (t (format "var %s: %s = %s;"
                  (plist-get var-data :var-name)
                  (plist-get var-data :zig-type)
                  (plist-get var-data :zig-value)))))))

(defun org-babel-zig-utility-header-to-zig ()
  "Generate a utility function to convert a column name
into a column number."

"
pub fn get_column_idx(header: [2][]const u8, column: []const u8) isize {
    for (header) |col, i| {
        if (std.mem.eql(u8, std.mem.span(col), std.mem.span(column))) {
            return @intCast(isize, i);
        }
    }
    return -1;
}
")

(defun org-babel-zig-header-to-zig (head type-descriptor)
  "Convert an elisp list of header table into a zig vector
specifying a variable with the name of the table."
  (let ((table (car head))
        (headers (cdr head))
        (zig-type (plist-get type-descriptor :zig-type)))
    (concat
     (format
      "const %s_header = [%s][]const u8{%s};"
      table
      (length headers)
      (mapconcat (lambda (h) (format "%S" h)) headers ","))
     "\n"
     (format
      "
pub fn %s_h (row: usize, col: []const u8) %s {
    return %s[row][@intCast(usize, get_column_idx(%s_header,col))];
}
"
      table zig-type table table))))

(provide 'ob-zig)
;;; ob-zig.el ends here
