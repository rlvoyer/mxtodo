;;; mxtodo.el --- Library for managing TODOs with Markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Robert Voyer.

;; Author: Robert Voyer <robert.voyer@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (f "0.20.0") (ts "0.2"))
;; Keywords: calendar, convenience
;; URL: https://github.com/rlvoyer/mxtodo

;; This program is free software: you can redistribute it and/or modify it under the terms of the
;; GNU General Public License as published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
;; the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with this program.  If
;; not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a TODO buffer and buffer-local functionality for managing TODOs in
;; Markdown notes files.

;;; Code:

(require 'f)
(require 'ts)

(defgroup mxtodo nil
  "mxtodo Markdown TODO manager."
  :group 'tools)

(defcustom mxtodo-file-extension ".md"
  "The file extension for TODO files."
  :type 'string
  :group 'mxtodo)

(defcustom mxtodo-folder-path "~/Documents/Notes"
  "The folder where TODO files reside."
  :type 'string
  :group 'mxtodo)

(defconst mxtodo-buffer-name "*todo-list*"
  "The name of the TODO search results buffer.")

(cl-defstruct mxtodo-item
  "A data struct for TODO information."
  (file-path nil :readonly t :type string)
  (file-line-number nil :readonly t :type integer)
  (file-display-date-ts nil :readonly t :type ts)
  (file-last-update-ts nil :readonly t :type ts)
  (date-due-ts nil :type ts)
  (text nil :type string)
  (is-completed nil :type boolean))

(defun mxtodo--grep-command (folder-path)
  "The grep command used to find TODOs."
  (format "rg --json \"^- \\[(x|\\s)\\] (.*?)$\" %s" folder-path))

(defun mxtodo--gather-todos-tmpfile (folder-path)
  "Gather TODO items from mxtodo-folder-path and write to a temporary file."
  (let ((tmp-file-1 (make-temp-file "results"))
        (tmp-file-2 (make-temp-file "filetimestamps"))
        (tmp-file-3 (make-temp-file "todos")))
    (let ((full-command
           (concat
            (mxtodo--grep-command folder-path) " | "
            (format "%s" "jq -r -c 'select(.type==\"match\")|[.data.path.text, .data.line_number, .data.submatches[0].match.text]|@tsv' | ")
            (format "tee %s" tmp-file-1) " | "
            "cut -f1 | xargs stat -f %c "
            (format "> %s" tmp-file-2))))
      (shell-command full-command)
      (shell-command (format "paste %s %s | sort -t$'\t' -k4 -nr > %s" tmp-file-1 tmp-file-2 tmp-file-3))
      tmp-file-3)))

(defun mxtodo--ts-date-from-string (date-str)
  "Parse a date of the form YYYY-M-D into a ts date struct."
  (if (not (equal date-str nil))
      (let ((parts (split-string date-str "-")))
        (if (equal (length parts) 3)
            (make-ts
             :year (string-to-number (pop parts))
             :month (string-to-number (pop parts))
             :day (string-to-number (pop parts))
             :hour 0
             :minute 0
             :second 0)
          nil))
    nil))

(defun mxtodo--display-date-from-file-path (file-path)
  "Parse a TODO filename of the form /PATH/TO/FILE/YYYY-M-D.extension into a ts date."
  (let ((display-date-str (file-name-sans-extension (car (last (split-string file-path "/"))))))
    (mxtodo--ts-date-from-string display-date-str)))

(defun mxtodo--render-date (date)
  "Render a TODO date as a string."
  (if (not (equal date nil))
      (ts-format "%Y-%-m-%-d" date)
    ""))

(defun mxtodo--render-due-date (date)
  "Render a TODO due date as a string."
  (if (not (equal date nil))
      (format "due %s" (mxtodo--render-date date))
    ""))
  
(defun mxtodo--render-create-date (date)
  "Render a TODO create date as a string."
  (if (not (equal date nil))
      (format "created %s" (mxtodo--render-date date))
    ""))

(defun mxtodo--render-is-completed (todo)
  "Render a checkbox indicating whether TODO is completed."
  (if (mxtodo-item-is-completed todo) "- [x]" "- [ ]"))

(defface mxtodo--completed-face
  '((t
     :foreground "#6F6F6F"
     :strike-through t
     ))
  "Face for completed TODO items."
  :group 'mxtodo)

(defun mxtodo--prettify-text (todo todo-str start-pos end-pos)
  "Add properties to string TODO-STR."
  (if (mxtodo-item-is-completed todo)
      (progn
        (add-text-properties start-pos end-pos '(face mxtodo--completed-face) todo-str)
        todo-str)
    todo-str))

(defun mxtodo--visible-text (todo)
  "Construct the visible text portion of TODO text."
  (if (mxtodo-item-is-completed todo)
      (format "%s %s"
              (mxtodo--render-is-completed todo)
              (mxtodo-item-text todo))
    (format "%s %s (%s / %s)"
            (mxtodo--render-is-completed todo)
            (mxtodo-item-text todo)
            (mxtodo--render-create-date (mxtodo-item-file-display-date-ts todo))
            (mxtodo--render-due-date (mxtodo-item-date-due-ts todo)))))

(defun mxtodo--render-todo (todo)
  "Render a TODO as a string. This string includes an invisible portion."
  (let* ((visible-text (mxtodo--visible-text todo))
         (invisible-text (prin1-to-string todo))
         (line-text (format "%s\t%s" visible-text invisible-text))
         (invisible-start-pos (length visible-text))
         (invisible-end-pos (length line-text))
         (todo-text-start-pos 6)
         (todo-text-end-pos (+ todo-text-start-pos (length (mxtodo-item-text todo)))))
    (mxtodo--render-create-date (mxtodo-item-file-display-date-ts todo))
    (put-text-property invisible-start-pos invisible-end-pos 'invisible t line-text)
    (mxtodo--prettify-text todo line-text todo-text-start-pos todo-text-end-pos)))

(defun mxtodo--extract-info-from-text (todo-line)
  "Extract a 3-element vector containing an is-completed bool, the TODO text, and a due date from a string of the form `- [ ] do something useful (due 2021-7-2)`."
  (if (string-match "^- \\[\\(x\\|[[:blank:]]\\)\\] \\(.*?\\)\\(?: (due \\(.*\\))\\)?$" todo-line)
      (let* ((completed-text (match-string 1 todo-line))
             (todo-text (match-string 2 todo-line))
             (is-completed (equal completed-text "x"))
             (due-date (mxtodo--ts-date-from-string (match-string 3 todo-line))))
        (vector todo-text is-completed due-date))
    (vector todo-line nil nil)))

(defun mxtodo--make-todo-from-temp-file-line (line)
  "Parse a TODO temp file line and construct a todo-item."
  (let* ((parts (split-string line "\t"))
         (file-path (pop parts))
         (file-line-number (string-to-number (pop parts)))
         (file-display-date-ts (mxtodo--display-date-from-file-path file-path))
         (todo-text (pop parts))
         (file-last-update-ts (make-ts :unix (string-to-number (pop parts)))))
    (seq-let [todo-text is-completed date-due] (mxtodo--extract-info-from-text todo-text)
             (make-mxtodo-item
              :file-path file-path
              :file-line-number file-line-number
              :file-display-date-ts file-display-date-ts
              :file-last-update-ts file-last-update-ts
              :text todo-text
              :is-completed is-completed
              :date-due-ts date-due))))

(defun mxtodo--todo-completed-p (todo-text)
  "Determine if the current TODO item is completed."
  (not (null (string-match-p "^- \\[x\\]" todo-text))))

(defun mxtodo--toggle-todo-completed (todo)
  "Toggle a TODO's is-completed field."
  (progn
    (setf
     (mxtodo-item-is-completed todo)
     (not
      (mxtodo--todo-completed-p
       (mxtodo-item-text todo))))
    todo))

(defun mxtodo--find-invisible-region-in-line ()
  "Return the beginning point of invisible region on the current line."
  (save-excursion
    (progn
      (beginning-of-line)
      (next-single-property-change (point) 'invisible))))

(defun mxtodo--read-todo-from-line ()
  "Get the TODO item on the current line."
  (car
   (read-from-string
    (buffer-substring
     (mxtodo--find-invisible-region-in-line) (line-end-position)))))

(defun mxtodo--delete-current-line ()
  "Delete (not kill) the current line."
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun mxtodo--due-date-str (todo)
  "Serialize TODO due date."
  (let ((date (mxtodo-item-date-due-ts todo)))
    (if (not (equal date nil))
        (format " (due %s)" (mxtodo--render-date date))
    "")))

(defun mxtodo--todo-str (todo)
  "Render a TODO as a string."
  (let* ((todo-line
          (format "%s %s%s"
                  (mxtodo--render-is-completed todo)
                  (mxtodo-item-text todo)
                  (mxtodo--due-date-str todo))))
    todo-line))

(defun mxtodo--write-todo-to-file (todo)
  "Persist a TODO from memory back to its source file."
  (with-current-buffer (find-file-noselect (mxtodo-item-file-path todo) t t)
    (goto-char (point-min))
    (forward-line (1- (mxtodo-item-file-line-number todo)))
    (mxtodo--delete-current-line)
    (insert (mxtodo--todo-str todo) "\n")
    (save-buffer)
    (kill-buffer)))

;;;###autoload
(define-derived-mode mxtodo-mode text-mode "Mxtodo"
  "Major mode for managing Markdown TODO items."
  :group 'mxtodo)

;;;###autoload
(defun mxtodo-make-todo-buffer (&optional buffer-name folder-path)
  "Construct a read-only buffer where each-line corresponds to a TODO item from `todo-items`."
  (interactive)
  (unless buffer-name (setq buffer-name mxtodo-buffer-name))
  (unless folder-path (setq folder-path mxtodo-folder-path))
  (let* ((temp-file-name (mxtodo--gather-todos-tmpfile folder-path))
         (temp-file-text (f-read-text temp-file-name))
         (todos (list)))
    (message (format "Reading TODOS from %s" temp-file-name))
    (dolist (line (split-string temp-file-text "\n"))
      (if (not (string= "" line))
          (setq todos (cons (mxtodo--make-todo-from-temp-file-line line) todos))))
    (setq todos (cl-sort
                 (copy-tree todos)
                 'ts>
                 :key (lambda (x) (mxtodo-item-file-display-date-ts x))))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (text-mode)
        (save-excursion
          (goto-char (point-min))
          (while todos
            (let ((todo (pop todos)))
              (insert (mxtodo--render-todo todo) "\n")))))
      (read-only-mode))
    (switch-to-buffer buffer-name)
    (mxtodo-mode)))

;;;###autoload
(defun mxtodo-toggle-current-todo-completed (&optional buffer-name)
  "Toggle the `completed` value of the TODO at point."
  (interactive)
  (unless buffer-name (setq buffer-name mxtodo-buffer-name))
  (with-current-buffer buffer-name
    (let ((todo (mxtodo--toggle-todo-completed (mxtodo--read-todo-from-line))))
      (mxtodo--write-todo-to-file todo)))
  nil)

(define-key mxtodo-mode-map (kbd "g") #'mxtodo-make-todo-buffer)
(define-key mxtodo-mode-map (kbd "X") #'mxtodo-toggle-current-todo-completed)
(define-key mxtodo-mode-map (kbd "q") #'kill-this-buffer)
(define-key mxtodo-mode-map (kbd "?") #'describe-mode)

(provide 'mxtodo)

;;; mxtodo.el ends here
