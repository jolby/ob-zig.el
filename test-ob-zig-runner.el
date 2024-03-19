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
(defvar ob-zig-setup-location nil)

(if load-in-progress
    (setq ob-zig-setup-location (symbol-file 'ob-zig-setup-location))
  (setq ob-zig-setup-location (buffer-file-name)))

;; Example from my doom-emacs setup
;; (add-to-list 'load-path (expand-file-name "straight/repos/org/testing" doom-local-dir))

(defvar ob-zig-test-dir (file-name-directory ob-zig-setup-location))

(defun test-ob-zig-id-files ()
  (interactive)
  (directory-files
   ob-zig-test-dir 'full
   "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$"))

(defun test-ob-zig-update-id-locations ()
  (interactive)
  (org-id-update-id-locations
   (test-ob-zig-id-files)))

(defun test-ob-zig-setup-tests ()
  (interactive)
  (condition-case no-org-test-error
      (progn
        (require 'org-test))
    (error
     (message "Error loading org-test. You must ensure the directory containing org-test.el is on the load-path. %s" (error-message-string no-org-test-error))))

  (if (and (stringp ob-zig-setup-location) (file-exists-p ob-zig-setup-location))
      (add-to-list 'load-path (file-name-directory  ob-zig-setup-location))
    (error "Required variable: ob-zig-setup-location [%s] not set or that directory doesn't exist! " ob-zig-setup-location))

  (require 'org-archive)
  (require 'ob-zig)
  (require 'test-ob-zig)

  (add-to-list 'org-babel-load-languages '(zig . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (test-ob-zig-update-id-locations))

(defun test-ob-zig-run-tests ()
  (interactive)
  (test-ob-zig-setup-tests)
  (ert "^ob-zig"))


(provide 'test-ob-zig-runner)
;;; test-ob-zig-runner.el ends here
