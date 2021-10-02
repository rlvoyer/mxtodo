;;; mxtodo.el --- Library for managing TODOs with Markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Robert Voyer.

;; Author: Robert Voyer <robert.voyer@gmail.com>
;; Version: 0.1.3
;; Package-Requires: ((emacs "27.1") (dash "2.19.0") (f "0.20.0") (ts "0.2"))
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

;; This package provides a TODO buffer and associated functionality for managing TODOs in Markdown
;; notes files.

;;; Code:

(require 'dash)
(require 'f)
(require 'ts)
(require 'xref)

(unless (functionp 'module-load)
  (error "Dynamic module feature not available, please compile Emacs --with-modules option turned on"))

;; if mxtodo-searcher.so not in same directory as load-file-name, download it
;; URL should be a function of architecture and version, but let's assume architecture to start
(eval-when-compile
  (message (concat "Using local mxtodo-searcher module: " (getenv "MXTODO_SEARCHER_LOCAL_MODULE_PATH")))

  (unless (file-exists-p "mxtodo-searcher.so")
    (progn
      (copy-file (getenv "MXTODO_SEARCHER_LOCAL_MODULE_PATH") (expand-file-name "mxtodo-searcher.so")))))

(require 'mxtodo-searcher)

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

(defcustom mxtodo-pattern-str "^- ?\\[[Xx ]\\]"
  "The PCRE regular expression pattern for matching TODO lines."
  :type 'string
  :group 'mxtodo)

(defconst mxtodo-buffer-name "*todo-list*"
  "The name of the TODO search results buffer.")

(defvar mxtodo-hide-completed nil
  "Whether to hide completed TODO items.")

(cl-defstruct mxtodo-item
  "A data struct for TODO information."
  (file-path nil :readonly t :type string)
  (file-line-number nil :readonly t :type integer)
  (file-display-date-ts nil :readonly t :type ts)
  (file-last-update-ts nil :readonly t :type ts)
  (date-due-ts nil :type ts)
  (text nil :type string)
  (is-completed nil :type boolean))

(defun mxtodo--gather-todos (&optional folder-path file-ext todo-pattern)
  "Gather todo items from files with extension FILE-EXT in FOLDER-PATH matching TODO-PATTERN."
  (progn
    (unless folder-path (setq folder-path mxtodo-folder-path))
    (unless file-ext (setq file-ext mxtodo-file-extension))
    (unless todo-pattern (setq todo-pattern mxtodo-pattern-str))
    (let ((folder-abs-path (expand-file-name (file-name-as-directory folder-path))))
      (mxtodo-searcher-search-directory folder-abs-path file-ext todo-pattern))))

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

(defface mxtodo--due-date-face
  '((t
     :foreground "#DFAF8F"
     :weight bold
     ))
  "Face for due dates."
  :group 'mxtodo)

(defun mxtodo--render-due-date (date)
  "Render a TODO due date as a string."
  (if (not (equal date nil))
      (let* ((due-date-str (format " // due %s" (mxtodo--render-date date)))
             (start-pos 0)
             (end-pos (length due-date-str)))
        (progn
          (add-text-properties start-pos end-pos '(face mxtodo--due-date-face) due-date-str)
          due-date-str))
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
    (format "%s %s%s"
            (mxtodo--render-is-completed todo)
            (mxtodo-item-text todo)
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

(defun mxtodo--file-last-modified (file-path)
  "Return the file last modified timestamp as a ts struct by inspecting the file-attributes of FILE-PATH."
  (make-ts :unix (float-time (file-attribute-modification-time (file-attributes file-path)))))

(defun mxtodo--make-todo-from-searcher-vec (todo-vec)
  "Construct a todo-item from 3-element TODO-VEC."
  (progn
    (unless (equal (length todo-vec) 3)
      (error "Expected a 3-element todo vec, but got %s" todo-vec))
    (let* ((file-path (elt todo-vec 0))
           (file-line-number (elt todo-vec 1))
           (file-display-date-ts (mxtodo--display-date-from-file-path file-path))
           (todo-text (elt todo-vec 2))
           (file-last-update-ts (mxtodo--file-last-modified file-path)))
      (seq-let [todo-text is-completed date-due] (mxtodo--extract-info-from-text todo-text)
        (make-mxtodo-item
         :file-path file-path
         :file-line-number file-line-number
         :file-display-date-ts file-display-date-ts
         :file-last-update-ts file-last-update-ts
         :text todo-text
         :is-completed is-completed
         :date-due-ts date-due)))))

(defun mxtodo--toggle-todo-completed (todo)
  "Toggle TODO's is-completed field."
  (progn
    (setf
     (mxtodo-item-is-completed todo)
     (not (mxtodo-item-is-completed todo)))
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

(defun mxtodo--todo-is-fresh-p (todo)
  "Check that TODO note file has not been updated since last read."
  (let ((file-path (mxtodo-item-file-path todo)))
    (progn
      (ts=
       (mxtodo-item-file-last-update-ts todo)
       (mxtodo--file-last-modified file-path)))))

(defun mxtodo--write-todo-to-file (todo)
  "Persist a TODO from memory back to its source file."
  (let ((todo-file-buffer-name))
    (progn
      (let ((todo-file-path (mxtodo-item-file-path todo)))
        (if (mxtodo--todo-is-fresh-p todo)
            (save-excursion
              (progn
                (find-file todo-file-path)
                (goto-char (point-min))
                (forward-line (1- (mxtodo-item-file-line-number todo)))
                (mxtodo--delete-current-line)
                (insert (mxtodo--todo-str todo) "\n")
                (save-buffer)))
          (error "the file containing TODO has been modified since the last read; refresh the todo buffer.")))
      (if (not (string= todo-file-buffer-name (buffer-name)))
          (kill-buffer todo-file-buffer-name)))))

;;;###autoload
(define-derived-mode mxtodo-mode text-mode "Mxtodo"
  "Major mode for managing Markdown TODO items."
  :group 'mxtodo)

(defun mxtodo--sort-todos (todos)
  "Sort TODOS, a list of todo items.

TODOS are sorted by creation date and partitioned by completion status,
with incomplete todo items first, followed by completed todo items."
  (let* ((todos-sorted
          (cl-sort (copy-tree todos) 'ts> :key (lambda (x) (mxtodo-item-file-display-date-ts x))))
         (todos-separated
          (-separate (lambda (todo) (not (mxtodo-item-is-completed todo))) todos-sorted)))
    (-flatten todos-separated)))

(defun mxtodo--make-todo-xref (todo)
  "Make an xref from TODO."
  (let* ((file (mxtodo-item-file-path todo))
         (line (mxtodo-item-file-line-number todo))
         (column 0)
         (location (xref-make-file-location file line column)))
    (xref-make "Source of todo" location)))

;;;###autoload
(defun mxtodo-jump-to-current-todo-source (&optional buffer-name)
  "Goto the source of the todo item on the current line."
  (interactive)
  (unless buffer-name (setq buffer-name mxtodo-buffer-name))
  (with-current-buffer buffer-name
    (let* ((todo (mxtodo--read-todo-from-line))
           (todo-xref (mxtodo--make-todo-xref todo)))
      (xref-pop-to-location todo-xref))))

;;;###autoload
(defun mxtodo-make-todo-buffer (&optional buffer-name folder-path file-ext todo-pattern)
  "Construct a read-only buffer BUFFER-NAME where each line corresponds to a todo from notes in FOLDER-PATH."
  (interactive)
  (progn
    (unless buffer-name (setq buffer-name mxtodo-buffer-name))
    (unless folder-path (setq folder-path mxtodo-folder-path))
    (unless file-ext (setq file-ext mxtodo-file-extension))
    (unless todo-pattern (setq todo-pattern mxtodo-pattern-str))
    (let* ((todos
            (mapcar
             (lambda (v) (mxtodo--make-todo-from-searcher-vec v))
             (mxtodo--gather-todos folder-path file-ext todo-pattern)))
           (sorted-todos (mxtodo--sort-todos todos)))
      (with-current-buffer (get-buffer-create buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (text-mode)
          (save-excursion
            (goto-char (point-min))
            (while sorted-todos
              (let ((todo (pop sorted-todos)))
                (if (not (and mxtodo-hide-completed (mxtodo-item-is-completed todo)))
                    (insert (mxtodo--render-todo todo) "\n"))))))
        (read-only-mode))
      (switch-to-buffer buffer-name)
      (mxtodo-mode))))

;;;###autoload
(defun mxtodo-toggle-current-todo-completed (&optional buffer-name)
  "Toggle the `completed` value of the TODO at point."
  (interactive)
  (unless buffer-name (setq buffer-name mxtodo-buffer-name))
  (with-current-buffer buffer-name
    (let ((todo (mxtodo--toggle-todo-completed (mxtodo--read-todo-from-line))))
      (progn
        (mxtodo--write-todo-to-file todo)
        (mxtodo-make-todo-buffer buffer-name))))
  nil)

(defun mxtodo--daily-note-filename (&optional folder-path)
  "Get the path to today's daily note file."
  (unless folder-path (setq folder-path mxtodo-folder-path))
  (let* ((todays-date-str (ts-format "%Y-%-m-%-d" (ts-now)))
         (buffer-name (concat todays-date-str ".md"))
         (file-name (concat (expand-file-name (file-name-as-directory folder-path)) buffer-name)))
    file-name))

;;;###autoload
(defun mxtodo-create-daily-note (&optional folder-path)
  "Create a new Markdown notes file for today."
  (interactive)
  (let* ((file-name (mxtodo--daily-note-filename folder-path))
         (todays-date-str (ts-format "%Y-%-m-%-d" (ts-now))))
    (progn
      (find-file file-name)
      (if (eq (buffer-size) 0)
          (progn
            (insert (concat "# " todays-date-str))
            (end-of-line)
            (open-line 1)))
      (save-buffer)
      file-name)))

(defun mxtodo--current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun mxtodo--parse-date (date-str)
  "Parse the specified date string into a ts or return an error if it failed to parse."
  (condition-case
      nil
      (cl-values (ts-parse-fill 'begin date-str) nil)
    (error
     (cl-values nil (format "Unable to parse specified date string %s; date must be ISO-8601-formatted." date-str)))))

;;;###autoload
(defun mxtodo-create-todo (&optional folder-path file-name buffer-name todo-text due-date-ts)
  "Add a todo to today's daily note, updating todo buffer with name BUFFER-NAME."
  (interactive)
  (save-excursion
    (progn
      (unless folder-path
        (setq folder-path mxtodo-folder-path))
      (unless file-name
        (setq file-name (mxtodo-create-daily-note folder-path)))
      (unless buffer-name
        (setq buffer-name mxtodo-buffer-name))
      (unless todo-text
        (setq todo-text (read-string "Enter the TODO text: ")))
      (unless due-date-ts
        (let ((due-date-read (read-string "(Optional) Enter a due date: ")))
          (if (not (string= "" due-date-read))
              (-let [(due-date-parsed err) (mxtodo--parse-date due-date-read)]
                (if (not (equal err nil))
                    (error err)
                  (setq due-date-ts due-date-parsed))))))
      (progn
        (goto-char (point-max))
        (if (not (mxtodo--current-line-empty-p))
            (progn
              (end-of-line)
              (open-line 1)
              (goto-char (point-max))))
        (let ((todo (make-mxtodo-item
                     :file-path file-name
                     :file-line-number (1+ (string-to-number (format-mode-line "%l")))
                     :file-display-date-ts (ts-now)
                     :file-last-update-ts (mxtodo--file-last-modified file-name)
                     :date-due-ts due-date-ts
                     :text todo-text
                     :is-completed nil)))
          (progn
            (newline)
            (mxtodo--write-todo-to-file todo)
            (mxtodo-make-todo-buffer buffer-name folder-path)
            todo))))))


;;;###autoload
(defun mxtodo-today ()
  "Open today's daily note and make it the current buffer."
  (interactive)
  (let ((file-name (mxtodo-create-daily-note)))
    (find-file file-name)))

;;;###autoload
(defun mxtodo-toggle-hide-completed (&optional buffer-name)
  "Toggle the global variable `mxtodo-hide-completed'."
  (interactive)
  (progn
    (unless buffer-name
      (setq buffer-name mxtodo-buffer-name))
    (if mxtodo-hide-completed
        (setq mxtodo-hide-completed nil)
      (setq mxtodo-hide-completed t))
    (mxtodo-make-todo-buffer buffer-name)
    mxtodo-hide-completed))

(define-key mxtodo-mode-map (kbd "g") #'mxtodo-make-todo-buffer)
(define-key mxtodo-mode-map (kbd "X") #'mxtodo-toggle-current-todo-completed)
(define-key mxtodo-mode-map (kbd "q") #'kill-this-buffer)
(define-key mxtodo-mode-map (kbd "?") #'describe-mode)
(define-key mxtodo-mode-map (kbd "+") #'mxtodo-create-todo)
(define-key mxtodo-mode-map (kbd "H") #'mxtodo-toggle-hide-completed)
(define-key mxtodo-mode-map [(meta .)] #'mxtodo-jump-to-current-todo-source)

(provide 'mxtodo)

;;; mxtodo.el ends here
