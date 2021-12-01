;;; test-ob-zig.el --- tests for ob-zig.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Joel Boehland
;;
;; Author: Joel Boehland <jboehland@gmail.com>
;; Created: September 27, 2021
;; Modified: September 27, 2021
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Unit tests for Org Babel Zig
;;
;;; Code:

(unless (featurep 'ob-zig)
  (signal 'missing-test-dependency "Support for Zig code blocks"))

(require 'org-test)
(require 'org-archive)
(require 'ob-zig)

(ert-deftest ob-zig/assert ()
  (should t))

(ert-deftest ob-zig/elisp-to-zig-types ()
  (should (= )))


(ert-deftest ob-zig/simple-program ()
  "Hello world program."
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "dc04e7e6-da1b-4ab5-9ee9-fd4d2e67dc3e"
                      (org-babel-next-src-block 1)
                      (should (equal "hello" (org-babel-execute-src-block))))))

(ert-deftest ob-zig/integer-var ()
  "Test of an integer variable."
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "dc04e7e6-da1b-4ab5-9ee9-fd4d2e67dc3e"
        (org-babel-next-src-block 2)
        (should (= 12 (org-babel-execute-src-block))))))

(ert-deftest ob-zig/two-integer-var ()
  "Test of two integer variables."
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "dc04e7e6-da1b-4ab5-9ee9-fd4d2e67dc3e"
        (org-babel-next-src-block 3)
        (should (= 22 (org-babel-execute-src-block))))))

(ert-deftest ob-zig/string-var ()
  "Test of a string input variable"
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "dc04e7e6-da1b-4ab5-9ee9-fd4d2e67dc3e"
        (org-babel-next-src-block 4)
        (should (equal "word 4" (org-babel-execute-src-block))))))

(ert-deftest ob-zig/list-var ()
  "Test of a list input variable"
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "2fabb5f3-b2fd-4048-aa41-727a45027ac3"
        (org-babel-next-src-block 1)
        (should (string= "abcdef2" (org-babel-execute-src-block))))))

(provide 'test-ob-zig)
;;; test-ob-zig.el ends here
