;;; -*- lexical-binding: t -*-

(require 'f)
(require 'ts)

(cl-defstruct todo-search-item
  (file-path nil :readonly t :type string)
  (file-line-number nil :readonly t :type integer)
  (file-display-date-ts nil :readonly t :type ts)
  (file-last-update-ts nil :readonly t :type ts)
  (date-due-ts nil :type ts)
  (text nil :type string)
  (is-completed nil :type boolean))

(defvar todo-search-file-extension ".md"
  "The file extension for TODO files.")

(defvar todo-search-folder-path "/Users/robertvoyer/Documents/Notes"
  "The folder where TODO files reside.")

(defconst todo-search-buffer-name "*todo-list*"
  "The name of the TODO search results buffer.")

(defvar todo-search--grep-command
  (format "rg --json \"^- \\[(x|\\s)\\] (.*?)$\" %s" todo-search-folder-path)
  "The grep command used to find TODOs.")

(defun todo-search--gather-todos-tmpfile ()
  "Gather TODO items from todo-search-folder-path and write to a temporary file."
  (let ((tmp-file-1 (make-temp-file "results"))
        (tmp-file-2 (make-temp-file "filetimestamps"))
        (tmp-file-3 (make-temp-file "todos")))
    (let ((full-command
           (concat
            todo-search--grep-command " | "
            (format "%s" "jq -r -c 'select(.type==\"match\")|[.data.path.text, .data.line_number, .data.submatches[0].match.text]|@tsv' | ")
            (format "tee %s" tmp-file-1) " | "
            "cut -f1 | xargs stat -f %c "
            (format "> %s" tmp-file-2))))
      (shell-command full-command)
      (shell-command (format "paste %s %s | sort -t$'\t' -k4 -nr > %s" tmp-file-1 tmp-file-2 tmp-file-3))
      tmp-file-3)))

(defun todo-search--ts-date-from-string (date-str)
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

(defun todo-search--display-date-from-file-path (file-path)
  "Parse a TODO filename of the form /PATH/TO/FILE/YYYY-M-D.extension into a ts date."
  (let ((display-date-str (file-name-sans-extension (first (last (split-string file-path "/"))))))
    (todo-search--ts-date-from-string display-date-str)))

(defun todo-search--render-date (date)
  "Render a TODO date as a string."
  (if (not (equal date nil))
      (ts-format "%Y-%-m-%-d" date)
    ""))

(defun todo-search--render-due-date (date)
  "Render a TODO due date as a string."
  (if (not (equal date nil))
      (format "due %s" (todo-search--render-date date))
    ""))
  
(defun todo-search--render-create-date (date)
  "Render a TODO create date as a string."
  (if (not (equal date nil))
      (format "created %s" (todo-search--render-date date))
    ""))

(defun todo-search--render-is-completed (is-completed)
  "Render a checkbox indicating whether the TODO is completed."
  (if is-completed "- [x]" "- [ ]"))

(defun todo-search--render-todo (todo)
  "Render a TODO as a string. This string includes an invisible portion."
  (let* ((visible-text
          (format "%s %s (%s / %s)"
                  (todo-search--render-is-completed (todo-search-item-is-completed todo))
                  (todo-search-item-text todo)
                  (todo-search--render-create-date (todo-search-item-file-display-date-ts todo))
                  (todo-search--render-due-date (todo-search-item-date-due-ts todo))))
         (invisible-text (prin1-to-string todo))
         (line-text (format "%s\t%s" visible-text invisible-text))
         (invisible-start-pos (length visible-text))
         (invisible-end-pos (length line-text)))
    (todo-search--render-create-date (todo-search-item-file-display-date-ts todo))
    (put-text-property invisible-start-pos invisible-end-pos 'invisible t line-text)
    line-text))

(defun todo-search--extract-info-from-text (todo-line)
  "Extract a 3-element vector containing an is-completed bool, the TODO text, and a due date from a string of the form `- [ ] do something useful (due 2021-7-2)`."
  ;;  (if (string-match "^- \\[\\(x\\|[[:blank:]]\\)\\] \\(.*\\)$" todo-line)
  (if (string-match "^- \\[\\(x\\|[[:blank:]]\\)\\] \\(.*?\\)\\(?: (due \\(.*\\))\\)?$" todo-line)
      (let* ((completed-text (match-string 1 todo-line))
             (todo-text (match-string 2 todo-line))
             (is-completed (equal completed-text "x"))
             (due-date (todo-search--ts-date-from-string (match-string 3 todo-line))))
        (vector todo-text is-completed due-date))
    (vector todo-line nil nil)))

(defun todo-search--make-todo-from-temp-file-line (line)
  "Parse a TODO temp file line and construct a todo-item."
  (let* ((parts (split-string line "\t"))
         (file-path (pop parts))
         (file-line-number (string-to-number (pop parts)))
         (file-display-date-ts (todo-search--display-date-from-file-path file-path))
         (todo-text (pop parts))
         (file-last-update-ts (make-ts :unix (string-to-number (pop parts)))))
    (seq-let [todo-text is-completed date-due] (todo-search--extract-info-from-text todo-text)
             (make-todo-search-item
              :file-path file-path
              :file-line-number file-line-number
              :file-display-date-ts file-display-date-ts
              :file-last-update-ts file-last-update-ts
              :text todo-text
              :is-completed is-completed
              :date-due-ts date-due))))

(defun todo-search-make-todo-buffer ()
  "Construct a read-only buffer where each-line corresponds to a TODO item from `todo-items`."
  (interactive)
  (let* ((temp-file-name (todo-search--gather-todos-tmpfile))
         (temp-file-text (f-read-text temp-file-name)))
    (message temp-file-name)
    (with-current-buffer (get-buffer-create todo-search-buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (text-mode)
        (save-excursion
          (goto-char (point-min))
          (dolist (line (split-string temp-file-text "\n"))
            (if (not (string= "" line))
                (let* ((todo (todo-search--make-todo-from-temp-file-line line)))
                  (insert (todo-search--render-todo todo) "\n"))))))
      (read-only-mode))))

(defun todo-search--todo-completed-p (todo-text)
  "Determine if the current TODO item is completed."
  (not (null (string-match-p "^- \\[x\\]" todo-text))))

(defun todo-search--toggle-todo-completed (todo)
  "Toggle a TODO's is-completed field."
  (progn
    (setf
     (todo-search-item-is-completed todo)
     (not
      (todo-search--todo-completed-p
       (todo-search-item-text todo))))
    todo))

(defun todo-search--find-invisible-region-in-line ()
  "Return the beginning point of invisible region on the current line."
  (save-excursion
    (with-current-buffer todo-search-buffer-name
      (progn
        (beginning-of-line)
        (next-single-property-change (point) 'invisible)))))

(defun todo-search--read-todo-from-line ()
  "Get the TODO item on the current line."
  (with-current-buffer todo-search-buffer-name
    (car
     (read-from-string
      (buffer-substring
       (todo-search--find-invisible-region-in-line) (line-end-position))))))

(defun todo-search--delete-current-line ()
  "Delete (not kill) the current line."
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))

(defun todo-search--todo-str (todo)
  "Render a TODO as a string."
  (let* ((todo-line
          (format "%s %s (%s)"
                  (todo-search--render-is-completed (todo-search-item-is-completed todo))
                  (todo-search-item-text todo)
                  (todo-search--render-due-date (todo-search-item-date-due-ts todo)))))
    todo-line))

(defun todo-search--write-todo-to-file (todo)
  "Persist a TODO from memory back to its source file."
  (with-current-buffer (find-file-noselect (todo-search-item-file-path todo) t t)
    (goto-line (todo-search-item-file-line-number todo))
    (todo-search--delete-current-line)
    (insert (todo-search--todo-str todo) "\n")
    (save-buffer)
    (kill-buffer)))

(defun todo-search-toggle-current-todo-completed ()
  "Toggle the `completed` value of the TODO at point."
  (interactive)
  (with-current-buffer todo-search-buffer-name
    (let ((todo (todo-search--toggle-todo-completed (todo-search--read-todo-from-line))))
      (todo-search--write-todo-to-file todo)))
  nil)

;; TODO
;; 1) add ability to add due date
;; 2) ensure keybindings work ("g" for refresh, "q" for killing the buffer)
;; 3) check file last update timestamp before modifying and fail if changed since TODO was last read
;; 4) colorize based on due date

(todo-search-make-todo-buffer)
;;(define-key map (kbd "g") 'todo-search-make-todo-buffer)

(provide 'todo-search)
