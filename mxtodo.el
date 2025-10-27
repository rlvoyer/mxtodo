;;; mxtodo.el --- Library for managing TODOs with Markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Robert Voyer.

;; Author: Robert Voyer <robert.voyer@gmail.com>
;; Version: 0.5.1
;; Package-Requires: ((emacs "29.1") (dash "2.19.0") (f "0.20.0") (ts "1.2.2"))
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
(require 'sqlite)

(unless (functionp 'module-load)
  (error "Dynamic module feature not available, please compile Emacs --with-modules option turned on"))

(eval-and-compile
  (defun mxtodo--trim-system-info (&optional sys-config)
    "Trim unnecessary info off the end of a system-configuration string."
    (progn
      (unless sys-config (setq sys-config system-configuration))
      (let* ((parts (vconcat (split-string sys-config "-")))
             (last-idx (- (length parts) 1))
             (architecture (aref parts 0))
             (distro-or-os (aref parts last-idx))
             (pc-or-apple (aref parts 1)))
	(cond
	 ((string-prefix-p "darwin" distro-or-os t) (concat (aref parts 0) "-" (aref parts 1) "-" "darwin"))
	 ((and (equal pc-or-apple "pc") (equal (aref parts 2) "linux")) (concat architecture "-" "unknown" "-" "linux" "-" distro-or-os))
	 (t sys-config)))))

  (defun mxtodo--lib-extension (&optional sys-config)
    "Determine the library extension given a system-configuration string."
    (progn
      (unless sys-config (setq sys-config system-configuration))
      (if (string-match-p (regexp-quote "darwin") sys-config)
          "dylib"
	"so")))

  (defvar mxtodo--module-install-dir
    (concat (expand-file-name user-emacs-directory) "mxtodo")
    "The directory where the native searcher module is to be installed.")

  (defvar mxtodo--db-path
    (concat (file-name-as-directory mxtodo--module-install-dir) "mxtodo.db")
    "The path to the SQLite database file for storing TODO metadata.")

  (defun mxtodo--make-module-install-dir ()
    "Make a directory where mxtodo will put the searcher native module."
    (progn
      (unless (file-exists-p mxtodo--module-install-dir)
        (make-directory mxtodo--module-install-dir))
      mxtodo--module-install-dir))

  (defun mxtodo--make-module-symlink (module-filename symlink-filename)
    "Make a directory where mxtodo will put the searcher native module."
    (progn
      (message "Symlinking searcher module file %s to %s" module-filename symlink-filename)
      (if (file-exists-p symlink-filename)
          (delete-file symlink-filename))
      (make-symbolic-link module-filename symlink-filename)))

  (let* ((mxtodo-version (package-get-version))
         (mxtodo-searcher-module-install-file (concat (file-name-as-directory (mxtodo--make-module-install-dir)) (format "mxtodo-searcher.so.%s" mxtodo-version)))
         (mxtodo-searcher-module-symlink (concat (file-name-as-directory (mxtodo--make-module-install-dir)) "mxtodo-searcher.so")))
    (progn
      (unless (file-exists-p mxtodo-searcher-module-install-file)
        (if (getenv "MXTODO_SEARCHER_LOCAL_MODULE_PATH")
            (progn
              (message (concat "Using local mxtodo-searcher module: " (getenv "MXTODO_SEARCHER_LOCAL_MODULE_PATH")))
              (message (concat "Copying local module to " mxtodo-searcher-module-install-file))
              (copy-file (getenv "MXTODO_SEARCHER_LOCAL_MODULE_PATH") mxtodo-searcher-module-install-file t)
              (mxtodo--make-module-symlink mxtodo-searcher-module-install-file mxtodo-searcher-module-symlink))
          (let* ((arch-id (mxtodo--trim-system-info))
                 (lib-ext (mxtodo--lib-extension))
                 (mxtodo-searcher-module-url
                  (format "https://github.com/rlvoyer/mxtodo/releases/download/v%s/libmxtodo_searcher.%s.%s" mxtodo-version arch-id lib-ext)))
            (progn
              (message (concat "Using release mxtodo-searcher module: " mxtodo-searcher-module-url))
              (url-copy-file mxtodo-searcher-module-url mxtodo-searcher-module-install-file)
              (mxtodo--make-module-symlink mxtodo-searcher-module-install-file mxtodo-searcher-module-symlink)))))
      (add-to-list 'load-path mxtodo--module-install-dir))))

(require 'mxtodo-searcher)

;;; Database functions

(defvar mxtodo--db nil
  "The SQLite database connection for TODO metadata.")

(defun mxtodo-db--init ()
  "Initialize the SQLite database with schema if needed."
  (unless mxtodo--db
    (setq mxtodo--db (sqlite-open mxtodo--db-path))
    (sqlite-execute
     mxtodo--db
     "CREATE TABLE IF NOT EXISTS todos (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        file_path TEXT NOT NULL,
        line_number INTEGER NOT NULL,
        text_hash TEXT NOT NULL,
        date_due INTEGER,
        date_completed INTEGER,
        date_created INTEGER NOT NULL,
        date_modified INTEGER NOT NULL,
        tags TEXT DEFAULT '[]',
        UNIQUE(file_path, line_number)
      )")
    (sqlite-execute mxtodo--db "CREATE INDEX IF NOT EXISTS idx_file_path ON todos(file_path)")
    (sqlite-execute mxtodo--db "CREATE INDEX IF NOT EXISTS idx_date_due ON todos(date_due)")
    (sqlite-execute mxtodo--db "CREATE INDEX IF NOT EXISTS idx_text_hash ON todos(file_path, text_hash)")
    ;; Migrate schema to add top-3 columns if they don't exist
    (mxtodo-db--migrate-top-three-columns mxtodo--db))
  mxtodo--db)

(defun mxtodo-db--migrate-top-three-columns (db)
  "Add top-3 columns to the database if they don't exist."
  (let* ((table-info (sqlite-select db "PRAGMA table_info(todos)"))
         (column-names (mapcar (lambda (row) (nth 1 row)) table-info))
         (has-is-top-three (member "is_top_three" column-names))
         (has-top-three-marked-ts (member "top_three_marked_ts" column-names)))
    (unless has-is-top-three
      (sqlite-execute db "ALTER TABLE todos ADD COLUMN is_top_three INTEGER DEFAULT 0"))
    (unless has-top-three-marked-ts
      (sqlite-execute db "ALTER TABLE todos ADD COLUMN top_three_marked_ts INTEGER"))))

(defun mxtodo-db--get-todo (file-path line-number)
  "Get TODO metadata from database by FILE-PATH and LINE-NUMBER."
  (let ((db (mxtodo-db--init)))
    (car (sqlite-select db "SELECT * FROM todos WHERE file_path = ? AND line_number = ?"
                        (list file-path line-number)))))

(defun mxtodo-db--get-todos-for-file (file-path)
  "Get all TODO metadata from database for FILE-PATH."
  (let ((db (mxtodo-db--init)))
    (sqlite-select db "SELECT * FROM todos WHERE file_path = ?" (list file-path))))

(defun mxtodo-db--find-by-hash (file-path text-hash)
  "Find TODO in database by FILE-PATH and TEXT-HASH (for detecting moved TODOs)."
  (let ((db (mxtodo-db--init)))
    (car (sqlite-select db "SELECT * FROM todos WHERE file_path = ? AND text_hash = ?"
                        (list file-path text-hash)))))

(defun mxtodo-db--insert (file-path line-number text-hash date-due date-completed tags &optional is-top-three top-three-marked-ts)
  "Insert a new TODO into the database."
  (let ((db (mxtodo-db--init))
        (now (truncate (ts-unix (ts-now))))
        (top-three-flag (if is-top-three 1 0)))
    (sqlite-execute
     db
     "INSERT INTO todos (file_path, line_number, text_hash, date_due, date_completed, date_created, date_modified, tags, is_top_three, top_three_marked_ts)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
     (list file-path line-number text-hash date-due date-completed now now (json-encode tags) top-three-flag top-three-marked-ts))))

(defun mxtodo-db--update (file-path line-number text-hash date-due date-completed tags)
  "Update an existing TODO in the database."
  (let ((db (mxtodo-db--init))
        (now (truncate (ts-unix (ts-now)))))
    (sqlite-execute
     db
     "UPDATE todos SET text_hash = ?, date_due = ?, date_completed = ?, date_modified = ?, tags = ?
      WHERE file_path = ? AND line_number = ?"
     (list text-hash date-due date-completed now (json-encode tags) file-path line-number))))

(defun mxtodo-db--delete (file-path line-number)
  "Delete a TODO from the database."
  (let ((db (mxtodo-db--init)))
    (sqlite-execute db "DELETE FROM todos WHERE file_path = ? AND line_number = ?"
                    (list file-path line-number))))

(defun mxtodo-db--delete-todos-for-file (file-path)
  "Delete all TODOs for a specific file from the database."
  (let ((db (mxtodo-db--init)))
    (sqlite-execute db "DELETE FROM todos WHERE file_path = ?" (list file-path))))

(defun mxtodo-db--get-top-three-todos ()
  "Get all TODOs marked as top-3, sorted by timestamp."
  (let ((db (mxtodo-db--init)))
    (sqlite-select db "SELECT * FROM todos WHERE is_top_three = 1 ORDER BY top_three_marked_ts ASC")))

(defun mxtodo-db--set-top-three (file-path line-number timestamp)
  "Mark a TODO as one of the top 3 priorities."
  (let ((db (mxtodo-db--init)))
    (sqlite-execute
     db
     "UPDATE todos SET is_top_three = 1, top_three_marked_ts = ? WHERE file_path = ? AND line_number = ?"
     (list timestamp file-path line-number))))

(defun mxtodo-db--unset-top-three (file-path line-number)
  "Remove top-3 status from a TODO."
  (let ((db (mxtodo-db--init)))
    (sqlite-execute
     db
     "UPDATE todos SET is_top_three = 0, top_three_marked_ts = NULL WHERE file_path = ? AND line_number = ?"
     (list file-path line-number))))

(defun mxtodo-db--count-top-three ()
  "Count how many TODOs are currently marked as top-3."
  (let ((db (mxtodo-db--init)))
    (caar (sqlite-select db "SELECT COUNT(*) FROM todos WHERE is_top_three = 1"))))

(defun mxtodo-db--clear-top-three-for-completed (file-path line-number)
  "Clear top-3 status for a completed TODO."
  (mxtodo-db--unset-top-three file-path line-number))

(defun mxtodo--enforce-top-three-limit ()
  "If more than 3 TODOs are marked as top-three, remove the oldest."
  (let ((top-three-list (mxtodo-db--get-top-three-todos)))
    (when (> (length top-three-list) 3)
      ;; List is already sorted by timestamp ASC, so first item is oldest
      (let* ((oldest (car top-three-list))
             (file-path (nth 1 oldest))   ; Column 1 is file_path
             (line-number (nth 2 oldest))) ; Column 2 is line_number
        (mxtodo-db--unset-top-three file-path line-number)))))

(defun mxtodo--compute-text-hash (text)
  "Compute SHA256 hash of TODO TEXT for change detection."
  (secure-hash 'sha256 text))

(defgroup mxtodo nil
  "mxtodo Markdown TODO manager."
  :group 'tools
  :link '(url-link "https://www.github.com/rlvoyer/mxtodo"))

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

(defcustom mxtodo-date-render-format "%Y-%-m-%-d"
  "The date format to use when rendering dates in the mxtodo buffer."
  :type 'string
  :group 'mxtodo)

(defcustom mxtodo-date-serialization-format "%Y-%m-%dT%H:%M:%S"
  "The date format to use when serializing dates in TODO files."
  :type 'string
  :group 'mxtodo)

(defconst mxtodo-buffer-name "*todo-list*"
  "The name of the TODO search results buffer.")

(defvar mxtodo-hide-completed nil
  "Whether to hide completed TODO items.")

(defvar mxtodo-hide-incomplete nil
  "Whether to hide incomplete TODO items.")

(cl-defstruct mxtodo-item
  "A data struct for TODO information."
  (file-path nil :readonly t :type string)
  (file-line-number nil :readonly t :type integer)
  (file-display-date-ts nil :readonly t :type ts)
  (file-last-update-ts nil :readonly t :type ts)
  (text nil :type string)
  (text-hash nil :type string)
  (is-completed nil :type boolean)
  (date-due-ts nil :type ts)
  (date-completed-ts nil :type ts)
  (date-created-ts nil :type ts)
  (date-modified-ts nil :type ts)
  (links '() :type list)
  (tags '() :type list)
  (is-top-three nil :type boolean)
  (top-three-marked-ts nil :type ts))

(defun mxtodo--gather-todos (&optional folder-path file-ext todo-pattern)
  "Gather todo items from files matching specified parameters.
If set, FOLDER-PATH indicates the folder to search within. Otherwise,
it defaults to `mxtodo-folder-path`.
If set, FILE-EXT indicates the file extension for files to consider in search.
Otherwise, it defaults to `mxtodo-file-extension`.
If set, TODO-PATTERN is a PCRE-compatible regular expression string
that corresponds to the TODO pattern to search for.
Otherwise, it defaults to `mxtodo-pattern-str`."
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
  "Parse a TODO filename (/PATH/TO/FILE/YYYY-M-D.extension) into a date."
  (let ((display-date-str (file-name-sans-extension (car (last (split-string file-path "/"))))))
    (mxtodo--ts-date-from-string display-date-str)))

(defun mxtodo--render-date (date)
  "Render a TODO date as a string for viewing in the TODO buffer."
  (if (not (equal date nil))
      (ts-format mxtodo-date-render-format date)
    ""))

(defface mxtodo--due-date-face
  '((t
     :foreground "#DFAF8F"
     :weight bold
     ))
  "Face for due dates."
  :group 'mxtodo)

(defface mxtodo--completed-date-face
  '((t
     :foreground "#28CD41"
     :weight bold
     ))
  "Face for completed at dates."
  :group 'mxtodo)

(defun mxtodo--render-completed-date (date)
  "Render a TODO completed date as a string."
  (if (not (equal date nil))
      (let* ((completed-date-str (format " // completed %s" (mxtodo--render-date date)))
             (start-pos 0)
             (end-pos (length completed-date-str)))
        (progn
          (add-text-properties start-pos end-pos '(face mxtodo--completed-date-face) completed-date-str)
          completed-date-str))
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

(defface mxtodo--tag-face
  '((t
     :foreground "#93E0E3"
     :weight bold
     ))
  "Face for tags."
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

(defun mxtodo--visible-text (todo)
  "Construct the visible text portion of TODO text."
  (let ((tags-str (if (mxtodo-item-tags todo)
                      (let ((formatted-tags (mapconcat (lambda (tag) (concat "#" tag))
                                                       (mxtodo-item-tags todo)
                                                       " ")))
                        (concat " " (propertize formatted-tags 'face 'mxtodo--tag-face)))
                    "")))
    (format "%s %s%s%s"
            (mxtodo--render-is-completed todo)
            (mxtodo-item-text todo)
            tags-str
            (if (mxtodo-item-is-completed todo)
                (mxtodo--render-completed-date (mxtodo-item-date-completed-ts todo))
              (mxtodo--render-due-date (mxtodo-item-date-due-ts todo))))))

(defface mxtodo--link-text-face
  '((t
     :foreground "#F0DFAF"
     :weight bold
     :underline t
     ))
  "Face for link text."
  :group 'mxtodo)

(defface mxtodo--link-url-face
  '((t
     :foreground "#CC9393"
     ))
  "Face for link URLs."
  :group 'mxtodo)

(defface mxtodo--top-three-face
  '((t
     :background "#4F4F2F"
     :weight bold
     :underline t
     ))
  "Face for top 3 priority TODO items."
  :group 'mxtodo)

(defconst mxtodo--checkbox-offset 6
  "The length of the checkbox prefix on a rendered TODO item.")

(defun mxtodo--open-link-url (link-url)
  "A keymap for opening link URLs in a browser."
  (lambda ()
    (interactive)
    (browse-url link-url)))

(defun mxtodo--highlight-link (line-text link)
  "Make the specified string LINE-TEXT clickable around LINK."
  (let* ((link-text-start (+ mxtodo--checkbox-offset (cdr (assoc "text_start_offset" link))))
         (link-text-len (length (cdr (assoc "text" link))))
         (link-text-end (+ link-text-start link-text-len))
         (lp (list 'face 'mxtodo--link-text-face
                   'font-lock-multiline t))
         (url-text (cdr (assoc "url" link)))
         (url-text-start (+ mxtodo--checkbox-offset (cdr (assoc "url_start_offset" link))))
         (url-text-len (length (cdr (assoc "url" link))))
         (url-text-end (+ url-text-start url-text-len))
         (up (list 'face 'mxtodo--link-url-face
                   'font-lock-multiline t))
         (span-start (+ mxtodo--checkbox-offset (cdr (assoc "start_offset" link))))
         (span-len (cdr (assoc "length" link)))
         (span-end (+ span-start span-len))
         (km (make-sparse-keymap))
         (link-opener (mxtodo--open-link-url url-text)))
    (progn
      (add-text-properties link-text-start link-text-end lp line-text)
      (add-text-properties url-text-start url-text-end up line-text)
      (define-key km (kbd "o") link-opener)
      (put-text-property span-start span-end 'keymap km line-text))))

(defun mxtodo--highlight-markdown-links (line-text links)
  "Make the specified string LINE-TEXT clickable around all links in list LINKS."
  (mapcar
   (lambda (link) (mxtodo--highlight-link line-text link))
   links))

(defun mxtodo--render-todo (todo)
  "Render a TODO as a string. This string includes an invisible portion."
  (let* ((visible-text (mxtodo--visible-text todo))
         (invisible-text (prin1-to-string todo))
         (line-text (format "%s\t%s" visible-text invisible-text))
         (invisible-start-pos (length visible-text))
         (invisible-end-pos (length line-text))
         (todo-text-start-pos 6)
         (todo-text-end-pos (+ todo-text-start-pos (length (mxtodo-item-text todo)))))
    (progn
      (put-text-property invisible-start-pos invisible-end-pos 'invisible t line-text)
      (if (mxtodo-item-is-completed todo)
          (add-text-properties todo-text-start-pos todo-text-end-pos '(face mxtodo--completed-face) line-text))
      ;; Apply top-3 background to entire visible portion
      (when (mxtodo-item-is-top-three todo)
        (add-text-properties 0 invisible-start-pos '(face mxtodo--top-three-face) line-text))
      (mxtodo--highlight-markdown-links line-text (mxtodo-item-links todo))
      line-text)))

(defun mxtodo--extract-info-from-text (todo-line)
  "Parse TODO-LINE string into a 4-element todo vector.
The resulting vector contains an is-completed bool, the TODO text,
a due date, and a completed date.
Consider the example todo-line: `- [ ] do something useful (due 2021-7-2)`.
The resulting vector would contain [nil \"do something useful\" #s(ts ...) nil]."
  (let* ((_ (string-match "^- \\[\\(?1:x\\|[[:blank:]]\\)\\] \\(?2:.*?\\)\\(?: *(due \\(?3:[^)]+\\))\\)?\\(?: *(completed \\(?4:[^)]+\\))\\)?$" todo-line))
         (completed-text (match-string 1 todo-line))
         (todo-text (match-string 2 todo-line))
         (due-date-str (match-string 3 todo-line))
         (completed-date-str (match-string 4 todo-line))
         (due-date (mxtodo--ts-date-from-string due-date-str))
         (completed-date (mxtodo--ts-date-from-string completed-date-str))
         (is-completed (equal completed-text "x")))
    (vector todo-text is-completed due-date completed-date)))

(defun mxtodo--file-last-modified (file-path)
  "Return the file last modified timestamp of the file at FILE-PATH.
This function extracts the last-modified timestamp from file attributes.
The resulting timestamp is returned as a ts struct."
  (make-ts :unix (float-time (file-attribute-modification-time (file-attributes file-path)))))

(defun mxtodo--make-todo-with-reconciliation (todo-alist)
  "Create a TODO item from searcher alist, reconciling with database.
This handles the full flow: extract from alist, sync with DB,
construct final struct. Uses text hash to track TODOs across reordering."
  (let* ((file-path (cdr (assoc "file_path" todo-alist)))
         (line-number (cdr (assoc "line_number" todo-alist)))
         (file-display-date-ts (make-ts :unix (cdr (assoc "display_date" todo-alist))))
         (file-last-update-ts (mxtodo--file-last-modified file-path))
         (todo-text (cdr (assoc "text" todo-alist)))
         (text-hash (mxtodo--compute-text-hash todo-text))
         (is-completed (cdr (assoc "is_completed" todo-alist)))
         (links (cdr (assoc "links" todo-alist)))
         (tags-raw (or (cdr (assoc "tags" todo-alist)) '()))
         (tags (cond
                ((and (vectorp tags-raw) (> (length tags-raw) 0) (hash-table-p (aref tags-raw 0)))
                 (mapcar (lambda (tag) (gethash "text" tag)) (append tags-raw nil)))
                ((and (listp tags-raw) (> (length tags-raw) 0) (listp (car tags-raw)))
                 (mapcar (lambda (tag) (cdr (assoc "text" tag))) tags-raw))
                (t tags-raw)))
         (file-date-due (cdr (assoc "date_due" todo-alist)))
         (file-date-completed (cdr (assoc "date_completed" todo-alist)))
         (db-record-at-line (mxtodo-db--get-todo file-path line-number))
         (db-record-by-hash (mxtodo-db--find-by-hash file-path text-hash)))

    (cond
     ;; Case 1: Found at current line with matching hash - TODO hasn't moved
     ((and db-record-at-line (string= text-hash (nth 3 db-record-at-line)))
      (let ((db-date-due (nth 4 db-record-at-line))
            (db-date-completed (nth 5 db-record-at-line))
            (db-date-created (nth 6 db-record-at-line))
            (db-date-modified (nth 7 db-record-at-line))
            (db-tags (let ((parsed (json-parse-string (nth 8 db-record-at-line) :array-type 'list)))
                       (if (eq parsed :null) '() parsed)))
            (db-is-top-three (nth 9 db-record-at-line))
            (db-top-three-marked-ts (nth 10 db-record-at-line)))
        (make-mxtodo-item
         :file-path file-path
         :file-line-number line-number
         :file-display-date-ts file-display-date-ts
         :file-last-update-ts file-last-update-ts
         :text todo-text
         :text-hash text-hash
         :is-completed is-completed
         :date-due-ts (if db-date-due (make-ts :unix db-date-due) nil)
         :date-completed-ts (if db-date-completed (make-ts :unix db-date-completed) nil)
         :date-created-ts (if db-date-created (make-ts :unix db-date-created) nil)
         :date-modified-ts (if db-date-modified (make-ts :unix db-date-modified) nil)
         :links links
         :tags db-tags
         :is-top-three (and db-is-top-three (= db-is-top-three 1))
         :top-three-marked-ts (if db-top-three-marked-ts (make-ts :unix db-top-three-marked-ts) nil))))

     ;; Case 2: Found by hash at different line - TODO moved, update line number
     ((and db-record-by-hash (not (= line-number (nth 2 db-record-by-hash))))
      (let ((old-line-number (nth 2 db-record-by-hash))
            (db-date-due (nth 4 db-record-by-hash))
            (db-date-completed (nth 5 db-record-by-hash))
            (db-date-created (nth 6 db-record-by-hash))
            (db-date-modified (nth 7 db-record-by-hash))
            (db-tags (let ((parsed (json-parse-string (nth 8 db-record-by-hash) :array-type 'list)))
                       (if (eq parsed :null) '() parsed)))
            (db-is-top-three (nth 9 db-record-by-hash))
            (db-top-three-marked-ts (nth 10 db-record-by-hash)))
        ;; Delete old position
        (mxtodo-db--delete file-path old-line-number)
        ;; Delete whatever is at the current line if it exists and is different
        (when (and db-record-at-line (not (string= text-hash (nth 3 db-record-at-line))))
          (mxtodo-db--delete file-path line-number))
        ;; Insert at new position with preserved top-3 status
        (mxtodo-db--insert file-path line-number text-hash db-date-due db-date-completed db-tags
                          (and db-is-top-three (= db-is-top-three 1)) db-top-three-marked-ts)
        (make-mxtodo-item
         :file-path file-path
         :file-line-number line-number
         :file-display-date-ts file-display-date-ts
         :file-last-update-ts file-last-update-ts
         :text todo-text
         :text-hash text-hash
         :is-completed is-completed
         :date-due-ts (if db-date-due (make-ts :unix db-date-due) nil)
         :date-completed-ts (if db-date-completed (make-ts :unix db-date-completed) nil)
         :date-created-ts (if db-date-created (make-ts :unix db-date-created) nil)
         :date-modified-ts (if db-date-modified (make-ts :unix db-date-modified) nil)
         :links links
         :tags db-tags
         :is-top-three (and db-is-top-three (= db-is-top-three 1))
         :top-three-marked-ts (if db-top-three-marked-ts (make-ts :unix db-top-three-marked-ts) nil))))

     ;; Case 3: Different TODO at this line - old one was replaced
     (db-record-at-line
      ;; A different TODO is now at this line, the current TODO is new
      (let ((now (truncate (ts-unix (ts-now)))))
        ;; Delete the old TODO before inserting the new one
        (mxtodo-db--delete file-path line-number)
        (mxtodo-db--insert file-path line-number text-hash file-date-due file-date-completed tags)
        (make-mxtodo-item
         :file-path file-path
         :file-line-number line-number
         :file-display-date-ts file-display-date-ts
         :file-last-update-ts file-last-update-ts
         :text todo-text
         :text-hash text-hash
         :is-completed is-completed
         :date-due-ts (if file-date-due (make-ts :unix file-date-due) nil)
         :date-completed-ts (if file-date-completed (make-ts :unix file-date-completed) nil)
         :date-created-ts (make-ts :unix now)
         :date-modified-ts (make-ts :unix now)
         :links links
         :tags tags
         :is-top-three nil
         :top-three-marked-ts nil)))

     ;; Case 4: Completely new TODO
     (t
      (let ((now (truncate (ts-unix (ts-now)))))
        (mxtodo-db--insert file-path line-number text-hash file-date-due file-date-completed tags)
        (make-mxtodo-item
         :file-path file-path
         :file-line-number line-number
         :file-display-date-ts file-display-date-ts
         :file-last-update-ts file-last-update-ts
         :text todo-text
         :text-hash text-hash
         :is-completed is-completed
         :date-due-ts (if file-date-due (make-ts :unix file-date-due) nil)
         :date-completed-ts (if file-date-completed (make-ts :unix file-date-completed) nil)
         :date-created-ts (make-ts :unix now)
         :date-modified-ts (make-ts :unix now)
         :links links
         :tags tags
         :is-top-three nil
         :top-three-marked-ts nil))))))

(defun mxtodo-db--cleanup-orphaned-todos (file-path disk-todos)
  "Remove TODOs from database that no longer exist in FILE-PATH.
DISK-TODOS is a list of alists representing current TODOs from disk."
  (let* ((db-todos (mxtodo-db--get-todos-for-file file-path))
         (disk-line-numbers (-map (lambda (todo) (cdr (assoc "line_number" todo))) disk-todos)))
    (dolist (db-record db-todos)
      (let ((db-line-number (nth 2 db-record)))
        (unless (member db-line-number disk-line-numbers)
          (mxtodo-db--delete file-path db-line-number))))))

(defun mxtodo--toggle-todo-completed (todo)
  "Toggle TODO's is-completed field and update database."
  (let* ((new-completed (not (mxtodo-item-is-completed todo)))
         (new-completed-ts (if new-completed (ts-now) nil))
         (new-completed-unix (if new-completed-ts (truncate (ts-unix new-completed-ts)) nil))
         (file-path (mxtodo-item-file-path todo))
         (line-number (mxtodo-item-file-line-number todo))
         (text-hash (mxtodo-item-text-hash todo))
         (date-due (mxtodo-item-date-due-ts todo))
         (date-due-unix (if date-due (truncate (ts-unix date-due)) nil))
         (tags (mxtodo-item-tags todo)))
    ;; Update the TODO struct
    (setf (mxtodo-item-is-completed todo) new-completed)
    (setf (mxtodo-item-date-completed-ts todo) new-completed-ts)
    ;; Update the database
    (mxtodo-db--update file-path line-number text-hash date-due-unix new-completed-unix tags)
    ;; Auto-remove top-3 status when completing a TODO
    (when new-completed
      (mxtodo-db--clear-top-three-for-completed file-path line-number))
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

(defun mxtodo--serialize-as-str (todo)
  "Serialize a TODO as a string."
  (format "%s %s\n"
          (mxtodo--render-is-completed todo)
          (mxtodo-item-text todo)))

(defun mxtodo--todo-is-fresh-p (todo)
  "Check that TODO note file has not been updated since last read."
  (let ((file-path (mxtodo-item-file-path todo)))
    (progn
      (ts=
       (mxtodo-item-file-last-update-ts todo)
       (mxtodo--file-last-modified file-path)))))

(defun mxtodo--persist-todo (todo)
  "Persist a TODO from memory back to its notes file."
  (progn
    (let ((todo-file-path (mxtodo-item-file-path todo)))
      (if (mxtodo--todo-is-fresh-p todo)
          (save-excursion
            (progn
              (find-file todo-file-path)
              (goto-char (point-min))
              (forward-line (1- (mxtodo-item-file-line-number todo)))
              (mxtodo--delete-current-line)
              (insert (mxtodo--serialize-as-str todo))
              (save-buffer)))
        (error "the file containing TODO has been modified since the last read; refresh the todo buffer.")))))

(defun mxtodo-set-due-date (&optional buffer-name)
  "Set or update the due date for the TODO at point."
  (interactive)
  (unless buffer-name (setq buffer-name mxtodo-buffer-name))
  (with-current-buffer buffer-name
    (let* ((todo (mxtodo--read-todo-from-line))
           (due-date-read (read-string "Enter a due date: "))
           (due-date-ts (if (not (string= "" due-date-read))
                            (-let [(due-date-parsed err) (mxtodo--parse-date due-date-read)]
                              (if (not (equal err nil))
                                  (error err)
                                due-date-parsed))
                          nil))
           (due-date-unix (if due-date-ts (truncate (ts-unix due-date-ts)) nil)))
      (mxtodo-db--update
       (mxtodo-item-file-path todo)
       (mxtodo-item-file-line-number todo)
       (mxtodo-item-text-hash todo)
       due-date-unix
       (if (mxtodo-item-date-completed-ts todo)
           (truncate (ts-unix (mxtodo-item-date-completed-ts todo)))
         nil)
       (mxtodo-item-tags todo))
      (mxtodo-make-todo-buffer buffer-name))))

(defun mxtodo-clear-due-date (&optional buffer-name)
  "Remove the due date from the TODO at point."
  (interactive)
  (unless buffer-name (setq buffer-name mxtodo-buffer-name))
  (with-current-buffer buffer-name
    (let* ((todo (mxtodo--read-todo-from-line)))
      (mxtodo-db--update
       (mxtodo-item-file-path todo)
       (mxtodo-item-file-line-number todo)
       (mxtodo-item-text-hash todo)
       nil  ;; clear due date
       (if (mxtodo-item-date-completed-ts todo)
           (truncate (ts-unix (mxtodo-item-date-completed-ts todo)))
         nil)
       (mxtodo-item-tags todo))
      (mxtodo-make-todo-buffer buffer-name))))

;;;###autoload
(define-derived-mode mxtodo-mode text-mode "Mxtodo"
  "Major mode for managing Markdown TODO items."
  :group 'mxtodo)

(defun mxtodo--sort-todos (todos)
  "Sort TODOS, a list of todo items.

Priority (top-3) TODOs appear first, sorted by when they were marked as priority.
Then incomplete non-priority TODOs sorted by creation date descending.
Finally completed TODOs sorted by completion date descending."
  (let* ((default-date (ts-apply :year 1970 (ts-now)))
         ;; First separate by completion status
         (todos-by-completion (-separate (lambda (todo) (not (mxtodo-item-is-completed todo))) todos))
         (todos-incomplete (car todos-by-completion))
         (todos-complete (nth 1 todos-by-completion))
         ;; Separate incomplete into priority and non-priority
         (incomplete-by-priority (-separate (lambda (todo) (mxtodo-item-is-top-three todo)) todos-incomplete))
         (todos-priority (car incomplete-by-priority))
         (todos-regular (nth 1 incomplete-by-priority))
         ;; Sort priority by top-three-marked timestamp (ascending - earliest marked first)
         (todos-priority-sorted
          (cl-sort (copy-tree todos-priority) 'ts< :key (lambda (x) (or (mxtodo-item-top-three-marked-ts x) default-date))))
         ;; Sort regular incomplete by creation date (descending)
         (todos-regular-sorted
          (cl-sort (copy-tree todos-regular) 'ts> :key (lambda (x) (mxtodo-item-file-display-date-ts x))))
         ;; Sort completed by completion date (descending), fallback to file display date
         (todos-completed-sorted
          (cl-sort (copy-tree todos-complete) 'ts> :key (lambda (x) (or (mxtodo-item-date-completed-ts x) (mxtodo-item-file-display-date-ts x))))))
    ;; Return: priority first, then regular incomplete, then completed
    (-flatten (list todos-priority-sorted todos-regular-sorted todos-completed-sorted))))

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

(defun mxtodo--show-todo-p (todo)
  "Determine whether a particular TODO should be included."
  (not
   (or
    (and mxtodo-hide-completed (mxtodo-item-is-completed todo))
    (and mxtodo-hide-incomplete (not (mxtodo-item-is-completed todo))))))

;;;###autoload
(defun mxtodo-make-todo-buffer (&optional buffer-name folder-path file-ext todo-pattern)
  "Construct a read-only todo buffer BUFFER-NAME.
If BUFFER-NAME is not set, it defaults to mxtodo-buffer-name.
If set, FOLDER-PATH indicates the folder to search within. Otherwise,
it defaults to `mxtodo-folder-path`.
If set, FILE-EXT indicates the file extension for files to consider in search.
Otherwise, it defaults to `mxtodo-file-extension`.
If set, TODO-PATTERN is a PCRE-compatible regular expression string
that corresponds to the TODO pattern to search for.
Otherwise, it defaults to `mxtodo-pattern-str`."
  (interactive)
  (progn
    (unless buffer-name (setq buffer-name mxtodo-buffer-name))
    (unless folder-path (setq folder-path mxtodo-folder-path))
    (unless file-ext (setq file-ext mxtodo-file-extension))
    (unless todo-pattern (setq todo-pattern mxtodo-pattern-str))
    (let* ((raw-todos (append (mxtodo--gather-todos folder-path file-ext todo-pattern) nil))
           (todos (mapcar
                   (lambda (v) (mxtodo--make-todo-with-reconciliation v))
                   raw-todos))
           ;; Group raw todos by file for cleanup
           (todos-by-file (-group-by (lambda (v) (cdr (assoc "file_path" v))) raw-todos))
           (sorted-todos (mxtodo--sort-todos todos)))
      ;; Clean up orphaned TODOs from database
      (dolist (file-group todos-by-file)
        (let ((file-path (car file-group))
              (file-todos (cdr file-group)))
          (mxtodo-db--cleanup-orphaned-todos file-path file-todos)))
      (with-current-buffer (get-buffer-create buffer-name)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (text-mode)
          (save-excursion
            (goto-char (point-min))
            (while sorted-todos
              (let ((todo (pop sorted-todos)))
                (if (mxtodo--show-todo-p todo)
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
        (mxtodo--persist-todo todo)
        (mxtodo-make-todo-buffer buffer-name))))
  nil)

;;;###autoload
(defun mxtodo-toggle-current-todo-top-three (&optional buffer-name)
  "Toggle the top-3 status of the TODO at point."
  (interactive)
  (unless buffer-name (setq buffer-name mxtodo-buffer-name))
  (with-current-buffer buffer-name
    (let* ((todo (mxtodo--read-todo-from-line))
           (file-path (mxtodo-item-file-path todo))
           (line-number (mxtodo-item-file-line-number todo))
           (is-top-three (mxtodo-item-is-top-three todo)))
      (if is-top-three
          ;; Remove from top-3
          (mxtodo-db--unset-top-three file-path line-number)
        ;; Add to top-3 with FIFO enforcement
        (progn
          (mxtodo-db--set-top-three file-path line-number (truncate (ts-unix (ts-now))))
          (mxtodo--enforce-top-three-limit)))
      ;; Refresh the buffer to show updated status
      (mxtodo-make-todo-buffer buffer-name)))
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
  "Parse the specified date string DATE-STR.
The result is returned as a ts.
If the specified date does not parse, an error is raised."
  (condition-case
      nil
      (cl-values (ts-parse-fill 'begin date-str) nil)
    (error
     (cl-values nil (format "Unable to parse specified date string %s; date must be ISO-8601-formatted." date-str)))))

(defun mxtodo--create-todo (&optional folder-path file-name todo-text due-date-ts)
  "Create a todo-item in the notefile named FILE-NAME in directory FOLDER-PATH."
  (interactive)
  (save-excursion
    (progn
      (unless folder-path
        (setq folder-path mxtodo-folder-path))
      (unless file-name
        (setq file-name (mxtodo-create-daily-note folder-path)))
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
            (mxtodo--persist-todo todo)
            (mxtodo-db--insert
             file-name
             (mxtodo-item-file-line-number todo)
             (mxtodo--compute-text-hash todo-text)
             (if due-date-ts (truncate (ts-unix due-date-ts)) nil)
             nil  ;; date-completed is nil for new TODOs
             '()) ;; tags is empty for new TODOs
            todo))))))

(defun mxtodo-add-tags (&optional buffer-name)
  "Add tags to the TODO at point.
  Prompts for comma-separated tag names to add to existing tags."
  (interactive)
  (unless buffer-name (setq buffer-name mxtodo-buffer-name))
  (with-current-buffer buffer-name
    (let* ((todo (mxtodo--read-todo-from-line))
           (current-tags (mxtodo-item-tags todo))
           (new-tags-str (read-string "Enter tags (comma-separated): "))
           (new-tags (mapcar (lambda (s) (string-trim s "^[ \t\n\r]*#*[ \t\n\r]*"))
                             (split-string new-tags-str ",")))
           (merged-tags (delete-dups (append current-tags new-tags))))
      (mxtodo-db--update
       (mxtodo-item-file-path todo)
       (mxtodo-item-file-line-number todo)
       (mxtodo-item-text-hash todo)
       (if (mxtodo-item-date-due-ts todo)
           (truncate (ts-unix (mxtodo-item-date-due-ts todo)))
         nil)
       (if (mxtodo-item-date-completed-ts todo)
           (truncate (ts-unix (mxtodo-item-date-completed-ts todo)))
         nil)
       merged-tags)
      (mxtodo-make-todo-buffer buffer-name))))

(defun mxtodo-remove-tag (&optional buffer-name)
    "Remove a tag from the TODO at point.
  Prompts for tag selection from the TODO's existing tags."
    (interactive)
    (unless buffer-name (setq buffer-name mxtodo-buffer-name))
    (with-current-buffer buffer-name
      (let* ((todo (mxtodo--read-todo-from-line))
             (current-tags (mxtodo-item-tags todo)))
        (if (null current-tags)
            (message "No tags to remove")
          (let* ((tag-to-remove (completing-read "Remove tag: " current-tags nil t))
                 (updated-tags (remove tag-to-remove current-tags)))
            (mxtodo-db--update
             (mxtodo-item-file-path todo)
             (mxtodo-item-file-line-number todo)
             (mxtodo-item-text-hash todo)
             (if (mxtodo-item-date-due-ts todo)
                 (truncate (ts-unix (mxtodo-item-date-due-ts todo)))
               nil)
             (if (mxtodo-item-date-completed-ts todo)
                 (truncate (ts-unix (mxtodo-item-date-completed-ts todo)))
               nil)
             updated-tags)
            (mxtodo-make-todo-buffer buffer-name))))))

;;;###autoload
(defun mxtodo-create-todo (&optional folder-path file-name buffer-name todo-text due-date-ts)
  "Add a todo to today's daily note, updating todo buffer with name BUFFER-NAME."
  (interactive)
  (let ((todo (mxtodo--create-todo folder-path file-name todo-text due-date-ts)))
    (progn
      (mxtodo-make-todo-buffer buffer-name folder-path)
      todo)))

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

(defun mxtodo-toggle-hide-incomplete (&optional buffer-name)
  "Toggle the global variable `mxtodo-hide-incomplete'."
  (interactive)
  (progn
    (unless buffer-name
      (setq buffer-name mxtodo-buffer-name))
    (if mxtodo-hide-incomplete
        (setq mxtodo-hide-incomplete nil)
      (setq mxtodo-hide-incomplete t))
    (mxtodo-make-todo-buffer buffer-name)
    mxtodo-hide-incomplete))

(define-key mxtodo-mode-map (kbd "g") #'mxtodo-make-todo-buffer)
(define-key mxtodo-mode-map (kbd "X") #'mxtodo-toggle-current-todo-completed)
(define-key mxtodo-mode-map (kbd "q") #'kill-this-buffer)
(define-key mxtodo-mode-map (kbd "?") #'describe-mode)
(define-key mxtodo-mode-map (kbd "+") #'mxtodo-create-todo)
(define-key mxtodo-mode-map (kbd "H") #'mxtodo-toggle-hide-completed)
(define-key mxtodo-mode-map (kbd "h") #'mxtodo-toggle-hide-incomplete)
(define-key mxtodo-mode-map (kbd "!") #'mxtodo-set-due-date)
(define-key mxtodo-mode-map (kbd "D") #'mxtodo-clear-due-date)
(define-key mxtodo-mode-map (kbd "#") #'mxtodo-add-tags)
(define-key mxtodo-mode-map (kbd "T") #'mxtodo-remove-tag)
(define-key mxtodo-mode-map (kbd "t") #'mxtodo-toggle-current-todo-top-three)
(define-key mxtodo-mode-map [(meta .)] #'mxtodo-jump-to-current-todo-source)

(provide 'mxtodo)

;;; mxtodo.el ends here
