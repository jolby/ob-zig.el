;;; test-ob-zig-runner.el --- Test runner for test-ob-zig.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 jboehland
;;
;; Author:  Joel Boehland <jboehland@gmail.com>
;; Created: September 27, 2021
;; Modified: September 27, 2021
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Test runner for test-ob-zig.el
;;
;;
;;; Code:

(defvar root-org-test-dir "/home/jboehland/repos/emacs/doom-emacs/.local/straight/repos/org/testing")
(defvar ob-zig-test-dir "/home/jboehland/repos/zig/ob-zig.el")

(defun test-ob-zig-id-files ()
  (interactive)
  (directory-files
   ob-zig-test-dir 'full
   "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$"))

(defun test-ob-zig-update-id-locations ()
  (interactive)
  (org-id-update-id-locations
   (test-ob-zig-id-files)))

(defun ot-update ()
  (interactive)
  (org-test-update-id-locations))

(defun test-ob-zig-setup-tests ()
  (interactive)
  (when (and (stringp root-org-test-dir) (file-exists-p root-org-test-dir))
    (add-to-list 'load-path root-org-test-dir))
  (add-to-list 'load-path (file-name-directory (buffer-file-name)))
  (require 'org-test)
  (require 'org-archive)
  (require 'ob-zig)
  (require 'test-ob-zig)

  (org-babel-do-load-languages (and (mapc (lambda (lang) (add-to-list 'org-babel-load-languages (cons lang t)))
                                          '(zig org))
                                    org-babel-load-languages)))

(provide 'test-ob-zig-runner)
;;; test-ob-zig-runner.el ends here
