;;; scratch.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 jboehland
;;
;; Author: jboehland <https://github.com/jboehland>
;; Maintainer: jboehland <jboehland@penguin>
;; Created: August 03, 2021
;; Modified: August 03, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/jboehland/scratch
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:zig' function below.
(defun org-babel-expand-body-orig:zig (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-zig)
  (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-zig-var-to-zig (cdr pair))))
      vars "\n") "\n" body "\n")))


(defun org-babel-zig-var-to-zig (var)
  "Convert an elisp var into a string of zig source code
specifying a var of the same value."
  (format "%S" var))

(provide 'scratch)
;;; scratch.el ends here
