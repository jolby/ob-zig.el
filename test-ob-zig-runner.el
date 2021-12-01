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

;; (defvar root-org-test-dir "/home/jboehland/repos/emacs/doom-emacs/.local/straight/repos/org/testing")
(defvar root-org-test-dir (expand-file-name "straight/repos/org/testing" doom-local-dir))
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

(defun ot-update ()
  (interactive)
  (org-test-update-id-locations))

(defun test-ob-zig-setup-tests ()
  (interactive)
  (if (and (stringp root-org-test-dir) (file-exists-p root-org-test-dir))
      (add-to-list 'load-path root-org-test-dir)
    (error "Required variable: root-org-test-dir [%s] not set or that directory doesn't exist! " root-org-test-dir))
  (if (and (stringp ob-zig-setup-location) (file-exists-p ob-zig-setup-location))
      (add-to-list 'load-path (file-name-directory  ob-zig-setup-location))
    (error "Required variable: ob-zig-setup-location [%s] not set or that directory doesn't exist! " ob-zig-setup-location))

  (require 'org-test)
  (require 'org-archive)
  (require 'ob-zig)
  (require 'test-ob-zig)

  (org-babel-do-load-languages (and (mapc (lambda (lang) (add-to-list 'org-babel-load-languages (cons lang t)))
                                          '(zig org))
                                    org-babel-load-languages))
  (test-ob-zig-update-id-locations))

(defun test-ob-zig-run-tests ()
  (interactive)
  (test-ob-zig-setup-tests)
  (ert "^ob-zig"))


(provide 'test-ob-zig-runner)
;;; test-ob-zig-runner.el ends here
