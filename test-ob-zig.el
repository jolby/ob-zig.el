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

(ert-deftest ob-zig/assert ()
  (should t))


(provide 'test-ob-zig)
;;; test-ob-zig.el ends here
