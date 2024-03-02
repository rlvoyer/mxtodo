;;; mxtodo.el --- Library for managing TODOs with Markdown -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Robert Voyer.

;; Author: Robert Voyer <robert.voyer@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (dash "2.19.0") (f "0.20.0") (ts "1.2.2"))
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

  (defconst mxtodo--version
    (with-temp-buffer
      (insert-file-contents (or load-file-name byte-compile-current-file))
      (goto-char (point-min))
      (if (search-forward ";; Version: " nil t)
          (buffer-substring-no-properties (point) (line-end-position))
        (progn
          (message "unable to find mxtodo version: %s" (buffer-string))
          (throw 'mxtodo-error-downloading-searcher "unable to determine version to download"))))
    "The version of this module.")

  (let* ((mxtodo-searcher-module-install-file (concat (file-name-as-directory (mxtodo--make-module-install-dir)) (format "mxtodo-searcher.so.%s" mxtodo--version)))
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
                  (format "https://github.com/rlvoyer/mxtodo/releases/download/v%s/libmxtodo_searcher.%s.%s" mxtodo--version arch-id lib-ext)))
            (progn
              (message (concat "Using release mxtodo-searcher module: " mxtodo-searcher-module-url))
              (url-copy-file mxtodo-searcher-module-url mxtodo-searcher-module-install-file)
              (mxtodo--make-module-symlink mxtodo-searcher-module-install-file mxtodo-searcher-module-symlink)))))
      (add-to-list 'load-path mxtodo--module-install-dir))))

(require 'mxtodo-searcher)

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
  (date-due-ts nil :type ts)
  (date-completed-ts nil :type ts)
  (text nil :type string)
  (is-completed nil :type boolean)
  (links '() :type list)
  (tags '() :type list))

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
  (format "%s %s%s"
          (mxtodo--render-is-completed todo)
          (mxtodo-item-text todo)
          (if (mxtodo-item-is-completed todo)
              (mxtodo--render-completed-date (mxtodo-item-date-completed-ts todo))
            (mxtodo--render-due-date (mxtodo-item-date-due-ts todo)))))

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

(defun mxtodo--highlight-tag (line-text tag)
  "Make the specified TAG text colorized in LINE-TEXT."
  (let* ((tag-start (+ mxtodo--checkbox-offset (cdr (assoc "start_offset" tag))))
         (tag-end (+ tag-start (cdr (assoc "length" tag))))
         (tp (list 'face 'mxtodo--tag-face
                   'font-lock-multiline t)))
    (progn
      (add-text-properties tag-start tag-end tp line-text))))

(defun mxtodo--highlight-tags (line-text tags)
  "Colorize each of the TAGS in the specified string LINE-TEXT."
  (mapcar
   (lambda (tag) (mxtodo--highlight-tag line-text tag))
   tags))

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
      (mxtodo--highlight-markdown-links line-text (mxtodo-item-links todo))
      (mxtodo--highlight-tags line-text (mxtodo-item-tags todo))
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

(defun mxtodo--make-todo-from-searcher-alist (todo-alist)
  "Construct a todo-item from mxtodo-searcher alist."
  (let* ((file-path (cdr (assoc "file_path" todo-alist)))
         (file-line-number (cdr (assoc "line_number" todo-alist)))
         (file-display-date-ts (make-ts :unix (cdr (assoc "display_date" todo-alist))))
         (file-last-update-ts (mxtodo--file-last-modified file-path))
         (todo-text (cdr (assoc "text" todo-alist)))
         (is-completed (cdr (assoc "is_completed" todo-alist)))
         (date-due-val (cdr (assoc "date_due" todo-alist)))
         (date-due (if date-due-val (make-ts :unix date-due-val) nil))
         (date-completed-val (cdr (assoc "date_completed" todo-alist)))
         (date-completed (if date-completed-val (make-ts :unix date-completed-val)))
         (links (cdr (assoc "links" todo-alist)))
         (tags (cdr (assoc "tags" todo-alist))))
    (make-mxtodo-item
     :file-path file-path
     :file-line-number file-line-number
     :file-display-date-ts file-display-date-ts
     :file-last-update-ts file-last-update-ts
     :text todo-text
     :is-completed is-completed
     :date-due-ts date-due
     :date-completed-ts date-completed
     :links links
     :tags tags)))

(defun mxtodo--toggle-todo-completed (todo)
  "Toggle TODO's is-completed field."
  (progn
    (setf
     (mxtodo-item-is-completed todo)
     (not (mxtodo-item-is-completed todo)))
    (if (mxtodo-item-is-completed todo)
        (setf (mxtodo-item-date-completed-ts todo) (ts-now))
      (setf (mxtodo-item-date-completed-ts todo) nil))
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

(defun mxtodo--serialize-date (datetime)
  "Serialize a ts date as an ISO-8601 datetime formatted string."
  (ts-format "%Y-%m-%dT%H:%M:%SZ" datetime))

(defun mxtodo--serialize-due-date-str (todo)
  "Serialize TODO due date."
  (let ((date (mxtodo-item-date-due-ts todo)))
    (if (not (equal date nil))
        (format " (due %s)" (mxtodo--serialize-date date))
      "")))

(defun mxtodo--serialize-completed-date-str (todo)
  "Serialize TODO due date."
  (let ((date (mxtodo-item-date-completed-ts todo)))
    (if (not (equal date nil))
        (format " (completed %s)" (mxtodo--serialize-date date))
      "")))

(defun mxtodo--serialize-as-str (todo)
  "Serialize a TODO as a string."
  (let* ((todo-line
          (format "%s %s%s%s\n"
                  (mxtodo--render-is-completed todo)
                  (mxtodo-item-text todo)
                  (mxtodo--serialize-due-date-str todo)
                  (mxtodo--serialize-completed-date-str todo))))
    todo-line))

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
          (-separate (lambda (todo) (not (mxtodo-item-is-completed todo))) todos-sorted))
         (todos-incomplete (car todos-separated))
         (todos-complete (nth 1 todos-separated))
         (default-date (ts-apply :year 1970 (ts-now)))
         (todos-completed-sorted (cl-sort (copy-tree todos-complete) 'ts> :key (lambda (x) (or (mxtodo-item-date-completed-ts x) default-date)))))
    (-flatten (list todos-incomplete todos-completed-sorted))))

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
    (let* ((todos
            (mapcar
             (lambda (v) (mxtodo--make-todo-from-searcher-alist v))
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
            todo))))))

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
(define-key mxtodo-mode-map [(meta .)] #'mxtodo-jump-to-current-todo-source)

(provide 'mxtodo)

;;; mxtodo.el ends here
