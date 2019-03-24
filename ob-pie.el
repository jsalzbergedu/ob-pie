;;; ob-pie.el --- org-babel functions for pie evaluation -*- lexical-binding: t -*-

;; Copyright (C) Jacob Salzberg

;; Author: your name here
;; Keywords: literate programming, reproducible research, pie
;; Homepage: https://orgmode.org
;; Version: 0.1.0

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

;; Provides org-babel definitions for pie, the language used
;; in the book the little typer.

;; There is no relevant configuration.

;;; Requirements:

;; One must have racket as well as pie installed to use this package

;;; Code:
(require 'subr-x)
(require 'cl-lib)

(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("pie" . ".rkt"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:pie '())

(defun org-babel-expand-body:pie (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  ;; Doesnt really do anything yet
  body)

(defun org-babel-execute:pie (body params)
  "Execute a block of pie code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing pie source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session name
         ;; since we need a session, we either find one, or set it to "default" :P
         (session-name (car (cl-remove-if-not (lambda (x) (eq (car x) :session)) processed-params)))
         (session-name (or (and (not (string= (cdr session-name) "none")) (cdr session-name)) "default"))
         (session (org-babel-pie-initiate-session session-name))
         ;; variables assigned for use in the block
         ;; (vars (second processed-params))
         ;; (result-params (third processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (fourth processed-params))
         ;; expand the body with `org-babel-expand-body:pie'
         (full-body (org-babel-expand-body:pie
                     body
                     nil))
         (result ""))
    (with-current-buffer (process-buffer session)
      (erase-buffer))

    (let ((tempfile (make-temp-file "ob-pie")))
      (with-temp-buffer
        (insert "#lang pie\n")
        (insert full-body)
        (insert "'org-babel-finished-evaluating\n")
        (write-region (point-min) (point-max) tempfile))
      (process-send-string session (format ",load %s\n" tempfile)))


    ;; wait for pie to finish evaluating
    (with-current-buffer (process-buffer session)
      (let ((flag t))
        (while flag
          (accept-process-output)
          (goto-char (point-min))
          (setq flag (not (re-search-forward "(the Atom 'org-babel-finished-evaluating)" nil t)))))
      ;; delete the extra characters. (we know how many extra characters there are
      ;; because they end with 'org-babel-finished-evaluating)\n >)
      (delete-region (- (point-max) 42) (point-max))
      (buffer-substring-no-properties (point-min) (point-max)))))

;; ;; This function should be used to assign any variables in params in
;; ;; the context of the session environment.
;; (defun org-babel-prep-session:pie (session params)
;;   "Prepare SESSION according to the header arguments specified in PARAMS."
;;   )

;; (defun org-babel-pie-var-to-pie (var)
;;   "Convert an elisp var into a string of pie source code
;; specifying a var of the same value."
;;   (format "%S" var))

(defun org-babel-pie--table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  results
  )

(defun org-babel-pie-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    (if-let ((s (get-process session)))
        s
      (let ((ret (make-process :name session
                               :buffer (get-buffer-create (concat "*" session "-pie-repl*"))
                               :command '("racket")
                               :noquery t))
            (flag t))
        ;; wait for the process to start
        (with-current-buffer (process-buffer ret)
          (while flag
            (accept-process-output)
            (if (and (char-after ) (= (char-after (point)) ?>))
                (setq flag nil)
              (forward-char))))
        ret))))

(provide 'ob-pie)
;;; ob-template.el ends here
