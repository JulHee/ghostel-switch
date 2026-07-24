;;; ghostel-switch.el --- Select and switch ghostel buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 JulHee

;; Author: JulHee
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (ghostel "0.45.0"))
;; Keywords: convenience terminals
;; URL: https://github.com/JulHee/ghostel-switch

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Quickly switch between and create Ghostel terminal buffers.
;;
;; Accepting the default (empty RET) always creates a new buffer.
;; Project-scoped buffers are named `*ghostel:<PROJECT>-<N>*'.

;; Usage:
;;   M-x ghostel-switch/switch-all         ;; switch/create among all buffers
;;   M-x ghostel-switch/switch-project     ;; switch/create project buffers
;;
;; Configuration:
;;   (setq ghostel-switch-project-name-max-length 10) ;; default 10
;;   (setq ghostel-switch-no-project-name "default") ;; default "default"

;;; Code:

(require 'project nil)
(require 'ghostel nil)

;;; Variables

(defvar ghostel-switch-no-project-name "default"
  "Fallback project name used when there is no current project.")

(defvar ghostel-switch-project-name-max-length 10
  "Maximum number of characters taken from the project name when generating
project-scoped ghostel buffer names of the form `ghostel-<PROJECT>-<N>'.")

(defvar ghostel-switch--buffer-history '()
  "History of ghostel buffer names created via this selector.")

;;; Helpers

(defun ghostel-switch--next-name ()
  "Return the next free non-project ghostel name (lowest unused number)."
  (let ((n 1))
    (while (get-buffer (format "*ghostel:%d*" n))
      (cl-incf n))
    (number-to-string n)))

(defun ghostel-switch--next-project-name ()
  "Return the next free project ghostel name (lowest unused number)."
  (let* ((proj-name (ghostel-switch--project-short-name))
         (n 1))
    (while (get-buffer (format "*ghostel:%s-%d*" proj-name n))
      (cl-incf n))
    (format "%s-%d" proj-name n)))

(defun ghostel-switch--project-short-name ()
  "Return the current project's name, truncated to the configured max length.
Falls back to `ghostel-switch-no-project-name' when there is no project."
  (let* ((proj (project-current))
         (root (and proj (project-root proj)))
         (raw  (if root
                   (file-name-nondirectory (directory-file-name root))
                 ghostel-switch-no-project-name))
         (name (if (string-empty-p raw)
                   ghostel-switch-no-project-name
                 raw))
         (max-len ghostel-switch-project-name-max-length))
    (if (and (integerp max-len)
             (> max-len 0)
             (> (length name) max-len))
        (substring name 0 max-len)
      name)))

(defun ghostel-switch--create-or-switch (name)
  "Switch to a ghostel buffer named NAME, creating it if it does not exist."
  (let ((buf-name (format "*ghostel:%s*" name)))
    (if (get-buffer buf-name)
        (switch-to-buffer buf-name)
      (ghostel name)
      ;; Rename buffer to correct naming scheme
      (rename-buffer buf-name))))

(defun ghostel-switch--select-and-switch (buffers name-fn prompt-label)
  "Prompt to select a ghostel buffer from BUFFERS.
An empty selection creates a new buffer. NAME-FN is called to obtain the
new buffer's name. PROMPT-LABEL defines input text"
  (let* ((names (mapcar #'buffer-name buffers))
         (preview (funcall name-fn))
         (selection (completing-read
                     (format "Switch %s (Enter for new '%s'): " prompt-label preview)
                     names nil nil nil
                     'ghostel-switch-buffer-history nil)))
    (if (string-empty-p selection)
        (ghostel-switch--create-or-switch (funcall name-fn))
      (switch-to-buffer (get-buffer selection)))))

;;; Commands

(defun ghostel-switch-switch-all ()
  "Switch between all open Ghostel buffers.
Pressing Enter with an empty selection creates a new Ghostel buffer."
  (interactive)
  (ghostel-switch--select-and-switch
   (ghostel--all-buffers) #'ghostel-switch--next-name "Ghostel"))

(defun ghostel-switch-switch-project ()
  "Switch between Ghostel buffers belonging to the current project.
Pressing Enter with an empty selection creates a new Ghostel buffer for this
project, named `ghostel-<PROJECT>-<N>'."
  (interactive)
  (ghostel-switch--select-and-switch
   (ghostel--project-buffers) #'ghostel-switch--next-project-name "Project Ghostel"))

(provide 'ghostel-switch)
;;; ghostel-switch.el ends here
