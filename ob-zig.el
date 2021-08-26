;;; ob-zig.el --- org-babel functions for zig evaluation

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

;; Org-Babel support for evaluating C, C++, D code.

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


(defun org-babel-expand-body:zig (body params &optional processed-params)
  "Expand a block of zig code with org-babel according to
its header arguments."
  (message "XXX org-babel-expand-body:zig!!!")
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
		   ;; :includes '("stdint.h" "string.h") gives us a list of
		   ;; symbols; convert those to strings.
		   (when (symbolp inc) (setq inc (symbol-name inc)))
                   (format "@cInclude(\"%s\");" inc))
		 c-includes "\n")
		;; defines
		(mapconcat
		 (lambda (inc) (format "@cDefine(%s);" inc))
		 (if (listp c-defines) c-defines (list c-defines)) "\n")
		;; namespaces
		(mapconcat
		 (lambda (inc) (format "usingnamespace %s;" inc))
		 using-namespaces
		 "\n")
		;; variables
		;; (mapconcat 'org-babel-zig-var-to-zig vars "\n")
		;; ;; table sizes
		;; (mapconcat 'org-babel-zig-table-sizes-to-zig vars "\n")
		;; ;; tables headers utility
		;; (when colnames
		;;   (org-babel-zig-utility-header-to-zig))
		;; ;; tables headers
		;; (mapconcat 'org-babel-zig-header-to-zig colnames "\n")
		;; body
		(if main-p
		    (org-babel-zig-ensure-main-wrap body)
		  body) "\n") "\n")))


(defun org-babel-zig-ensure-main-wrap (body)
  "Wrap BODY in a \"main\" function call if none exists."
  (if (string-match "pub fn[ \t\n\r]*main(.*)" body)
      body
    (format "pub fn main() void {\n%s\n}\n" body)))

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
    (message "BODY: %s" full-body)
    (message "CMD: %s" full-command)
    (with-temp-file tmp-src-file (insert full-body))
    (let ((results (org-babel-eval full-command "")))
      (message "CMD RES: %s" results)
      results)))

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
  (unless (string= session "none")
    ))

;; helper functions
(defun org-babel-zig-format-val (type val)
  "Handle the FORMAT part of TYPE with the data from VAL."
  (let ((format-data (cadr type)))
    (if (stringp format-data)
	(cons "" (format format-data val))
      (funcall format-data val))))

(defun org-babel-zig-val-to-zig-type (val)
  "Determine the type of VAL.
Return a list (TYPE-NAME FORMAT).  TYPE-NAME should be the name of the type.
FORMAT can be either a format string or a function which is called with VAL."
  (let* ((basetype (org-babel-zig-val-to-base-type val))
	 (type
	  (pcase basetype
	    (`integerp '("int" "%d"))
	    (`floatp '("double" "%f"))
	    (`stringp
	     (list
	      (if (eq org-babel-c-variant 'd) "string" "const char*")
	      "\"%s\""))
	    (_ (error "unknown type %S" basetype)))))
    (cond
     ((integerp val) type) ;; an integer declared in the #+begin_src line
     ((floatp val) type) ;; a numeric declared in the #+begin_src line
     ((and (listp val) (listp (car val))) ;; a table
      `(,(car type)
	(lambda (val)
	  (cons
	   (format "[%d][%d]" (length val) (length (car val)))
	   (concat
	    (if (eq org-babel-c-variant 'd) "[\n" "{\n")
	    (mapconcat
	     (lambda (v)
	       (concat
		(if (eq org-babel-c-variant 'd) " [" " {")
		(mapconcat (lambda (w) (format ,(cadr type) w)) v ",")
		(if (eq org-babel-c-variant 'd) "]" "}")))
	     val
	     ",\n")
	    (if (eq org-babel-c-variant 'd) "\n]" "\n}"))))))
     ((or (listp val) (vectorp val)) ;; a list declared in the #+begin_src line
      `(,(car type)
	(lambda (val)
	  (cons
	   (format "[%d]" (length val))
	   (concat
	    (if (eq org-babel-c-variant 'd) "[" "{")
	    (mapconcat (lambda (v) (format ,(cadr type) v)) val ",")
	    (if (eq org-babel-c-variant 'd) "]" "}"))))))
     (t ;; treat unknown types as string
      type))))

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

(defun org-babel-zig-var-to-zig (pair)
  "Convert an elisp val into a string of C code specifying a var
of the same value."
  ;; TODO list support
  (let ((var (car pair))
	(val (cdr pair)))
    (when (symbolp val)
      (setq val (symbol-name val))
      (when (= (length val) 1)
	(setq val (string-to-char val))))
    (let* ((type-data (org-babel-zig-val-to-zig-type val))
	   (type (car type-data))
	   (formatted (org-babel-zig-format-val type-data val))
	   (suffix (car formatted))
	   (data (cdr formatted)))
      (format "%s %s%s = %s;"
	      type
	      var
	      suffix
	      data))))

(defun org-babel-zig-table-sizes-to-zig (pair)
  "Create constants of table dimensions, if PAIR is a table."
  (when (listp (cdr pair))
    (cond
     ((listp (cadr pair)) ;; a table
      (concat
       (format "const int %s_rows = %d;" (car pair) (length (cdr pair)))
       "\n"
       (format "const int %s_cols = %d;" (car pair) (length (cadr pair)))))
     (t ;; a list declared in the #+begin_src line
      (format "const int %s_cols = %d;" (car pair) (length (cdr pair)))))))

(defun org-babel-zig-utility-header-to-zig ()
  "Generate a utility function to convert a column name
into a column number."
  (pcase org-babel-c-variant
    ((or `c `cpp)
     (concat
      (if (eq org-babel-c-variant 'c)
          "extern "
	"extern \"C\" ")
      "int strcmp (const char *, const char *);
int get_column_num (int nbcols, const char** header, const char* column)
{
  int c;
  for (c=0; c<nbcols; c++)
    if (strcmp(header[c],column)==0)
      return c;
  return -1;
}
"))
    (`d
     "int get_column_num (string[] header, string column)
{
  foreach (c, h; header)
    if (h==column)
      return to!int(c);
  return -1;
}
")))

(defun org-babel-zig-header-to-zig (head)
  "Convert an elisp list of header table into a C or D vector
specifying a variable with the name of the table."
  (let ((table (car head))
        (headers (cdr head)))
    (concat
     (format
      (pcase org-babel-c-variant
	((or `c `cpp) "const char* %s_header[%d] = {%s};")
	(`d "string %s_header[%d] = [%s];"))
      table
      (length headers)
      (mapconcat (lambda (h) (format "%S" h)) headers ","))
     "\n"
     (pcase org-babel-c-variant
       ((or `c `cpp)
	(format
	 "const char* %s_h (int row, const char* col) { return %s[row][get_column_num(%d,%s_header,col)]; }"
	 table table (length headers) table))
       (`d
	(format
	 "string %s_h (size_t row, string col) { return %s[row][get_column_num(%s_header,col)]; }"
	 table table table))))))

(provide 'ob-zig)
;;; ob-zig.el ends here
