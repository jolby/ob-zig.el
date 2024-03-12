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
  (should (equal 'integerp (org-babel-zig-val-to-base-type 1)))
  (should (equal 'floatp (org-babel-zig-val-to-base-type 1.5)))
  (should (equal 'stringp (org-babel-zig-val-to-base-type "FOO"))))

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

(ert-deftest ob-zig/simple-zig-test ()
  "Test of a string input variable"
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "b1d3335f-8731-43b8-9246-6e613f3be3a2"
        (org-babel-next-src-block 1)
        (should (equal "hello" (org-babel-execute-src-block))))))

(ert-deftest ob-zig/fn-zig-test ()
  "Test of a string input variable"
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "b1d3335f-8731-43b8-9246-6e613f3be3a2"
        (org-babel-next-src-block 2)
        (should (equal "hello" (org-babel-execute-src-block))))))

(ert-deftest ob-zig/mutli-tests-zig-test ()
  "Test of a string input variable"
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "b1d3335f-8731-43b8-9246-6e613f3be3a2"
        (org-babel-next-src-block 3)
        (should (equal "hellohello" (org-babel-execute-src-block))))))

(ert-deftest ob-zig/list-var ()
  "Test of a list input variable"
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "2df386b8-b2a4-449c-8945-1dacad34e95e"
        (org-babel-next-src-block 1)
        (should (string= "abcdef2" (org-babel-execute-src-block))))))

(ert-deftest ob-zig/integer-table ()
  "Test int input table"
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "4a52142b-02e6-416b-a281-dd85c4f69da3"
        (org-babel-next-src-block 1)
        (should (equal
                 '((1 2) (3 4) ("A1" 3) ("B0" 2))
                 (org-babel-execute-src-block))))))

(ert-deftest ob-zig/float-table ()
  "Test float input table"
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "e089fd7b-9634-450d-acb7-36a82168dc30"
        (org-babel-next-src-block 1)
        (should (equal
                 '((1.1 2.2) (3.5 4.7) ("A1" 3.5) ("B0" 2.2))
                 (org-babel-execute-src-block))))))

(ert-deftest ob-zig/mixed-table ()
  "Test mixed input table"
  (if (executable-find org-babel-zig-compiler)
      (org-test-at-id "e112bc2e-419a-4890-99c2-7ac4779531cc"
        (org-babel-next-src-block 1)
        (should (equal
                 '(("monday" 34)
                   ("tuesday" 41)
                   ("wednesday" 56)
                   ("thursday" 17)
                   ("friday" 12)
                   ("saturday" 7)
                   ("sunday" 4)
                   ("tuesday_qty" 41)
                   ("day_idx_4" "friday"))
                 (org-babel-execute-src-block))))))

(provide 'test-ob-zig)
;;; test-ob-zig.el ends here
