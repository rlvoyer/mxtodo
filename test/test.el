;;; -*- lexical-binding: t; -*-

;; This program is free software: you can redistribute it and/or modify it under the terms of the
;; GNU General Public License as published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
;; the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with this program.  If
;; not, see <https://www.gnu.org/licenses/>.

(require 'ert)
(require 'ts)

(require 'mxtodo)

(ert-deftest test-display-date-from-file-path ()
  (should (equal (mxtodo--display-date-from-file-path "/foo/bar/2021-5-24.md")
                 (make-ts
                  :year 2021
                  :month 5
                  :day 24
                  :hour 0
                  :minute 0
                  :second 0))))

(ert-deftest test-render-a-todo-date ()
  (let ((date (make-ts
               :year 2021
               :month 5
               :day 24
               :hour 0
               :minute 0
               :second 0)))
    (should (equal (mxtodo--render-date date)
                   "2021-5-24"))))

(cl-defstruct test-tuple
  text
  expected-extracted-info)

(ert-deftest test-extract-info-from-text-returns-due-date ()
  (let* ((actual (mxtodo--extract-info-from-text "- [ ] write more tests (due 2021-7-14)"))
         (expected-due-date
          (make-ts
           :year 2021
           :month 7
           :day 14
           :hour 0
           :minute 0
           :second 0))
         (expected (vector "write more tests" nil expected-due-date nil)))
    (should (equal expected actual))))

(ert-deftest test-extract-info-from-text-returns-completed-date ()
  (let* ((actual (mxtodo--extract-info-from-text "- [x] write more tests (completed 2021-7-15)"))
         (expected-completed-date
          (make-ts
           :year 2021
           :month 7
           :day 15
           :hour 0
           :minute 0
           :second 0))         
         (expected (vector "write more tests" t nil expected-completed-date)))
    (should (equal expected actual))))

(ert-deftest test-extract-info-from-text-returns-due-date-and-completed-date ()
  (let* ((actual (mxtodo--extract-info-from-text "- [x] write more tests (due 2021-7-14) (completed 2021-7-15)"))
         (expected-due-date
          (make-ts
           :year 2021
           :month 7
           :day 14
           :hour 0
           :minute 0
           :second 0))         
         (expected-completed-date
          (make-ts
           :year 2021
           :month 7
           :day 15
           :hour 0
           :minute 0
           :second 0))         
         (expected (vector "write more tests" t expected-due-date expected-completed-date)))
    (should (equal expected actual))))

(ert-deftest test-render-date ()
  (let* ((date
          (make-ts :year 2021
                   :month 6
                   :day 24
                   :hour 0
                   :minute 0
                   :second 0))
         (expected "2021-6-24")
         (actual (mxtodo--render-date date)))
    (should (equal expected actual))))

(ert-deftest test-render-todo-no-due-date ()
  (let* ((input-todo
          (make-mxtodo-item :file-path "/Users/robertvoyer/Documents/Notes/2021-6-24.md"
                            :file-line-number 10
                            :file-display-date-ts (make-ts :year 2021
                                                           :month 6
                                                           :day 24
                                                           :hour 0
                                                           :minute 0
                                                           :second 0)
                            :file-last-update-ts (ts-now)
                            :text "write some unit tests"
                            :is-completed nil))
         (expected "- [ ] write some unit tests")
         (actual (todo-text-no-properties (mxtodo--render-todo input-todo))))
    (should (equal expected actual))))

(ert-deftest test-render-todo-with-due-date ()
  (let* ((input-todo
          (make-mxtodo-item :file-path "/Users/robertvoyer/Documents/Notes/2021-6-24.md"
                            :file-line-number 10
                            :file-display-date-ts (make-ts :year 2021
                                                           :month 6
                                                           :day 24
                                                           :hour 0
                                                           :minute 0
                                                           :second 0)
                            :date-due-ts (make-ts :year 2021
                                                  :month 7
                                                  :day 1
                                                  :hour 0
                                                  :minute 0
                                                  :second 0)
                            :file-last-update-ts (ts-now)
                            :text "write some unit tests"
                            :is-completed nil))
         (expected "- [ ] write some unit tests // due 2021-7-1")
         (actual (todo-text-no-properties (mxtodo--render-todo input-todo))))
    (should (equal expected actual))))

(ert-deftest test-parse-bad-date-returns-cl-values-with-a-non-nil-error ()
  (let ((expected (cl-values nil "Unable to parse specified date string 2021-8-7; date must be ISO-8601-formatted."))
        (actual (mxtodo--parse-date "2021-8-7")))
    (should (equal expected actual))))

(ert-deftest test-parse-good-date-returns-cl-values ()
  (let* ((date-str "2021-08-07")
         (expected-error nil)
         (expected-year 2021)
         (expected-month 8)
         (expected-day 7)
         (actual (mxtodo--parse-date date-str))
         (actual-error (nth 1 actual))
         (actual-date (car actual))
         (actual-year (ts-year actual-date))
         (actual-month (ts-month actual-date))
         (actual-day (ts-day actual-date)))
    (progn
      (should (equal expected-error actual-error))
      (should (equal expected-year actual-year))
      (should (equal expected-month actual-month))
      (should (equal expected-day actual-day)))))


;; TODO: fix this test
;; (ert-deftest test-make-todo-from-temp-file-line ()
;;   "Tests that a TODO can be constructed from a temp file line."
;;   (let* ((notes-dir (make-test-notes-dir))
;;          (notes-file (make-test-notes-file notes-dir 1))
;;          (expected (make-mxtodo-item :file-path notes-file
;;                            :file-line-number 10
;;                            :file-display-date-ts (make-ts :year 2021
;;                                                           :month 6
;;                                                           :day 24
;;                                                           :hour 0
;;                                                           :minute 0
;;                                                           :second 0)
;;                            :file-last-update (mxtodo--file-last-modified notes-file)
;;                            :text "write some unit tests"
;;                            :is-completed t))
;;         (actual
;;          (mxtodo--make-todo-from-temp-file-line  "/Users/robertvoyer/Documents/Notes/2021-6-24.md	10	- [x] write some unit tests	1624637870")))
;;     (should (equal expected actual))))

(defun todo-text-no-properties (rendered-todo-item)
  "Test rendering RENDERED-TODO-ITEM as a string with no properties."
  (substring-no-properties rendered-todo-item 0 (next-single-property-change 0 'invisible rendered-todo-item)))

(defun nshuffle (sequence)
  "Shuffle SEQUENCE."
  (cl-loop for i from (length sequence) downto 2
           do (cl-rotatef (elt sequence (random i))
                          (elt sequence (1- i))))
  sequence)

(ert-deftest test-sorting-todos ()
  "Tests that a list of todos is correctly sorted."
  (let* ((completed-todo-1
          (make-mxtodo-item :file-path "/path/to/notes/2021-7-17.md"
                            :file-line-number 10
                            :file-display-date-ts (make-ts :year 2021
                                                           :month 7
                                                           :day 17
                                                           :hour 0
                                                           :minute 0
                                                           :second 0)
                            :file-last-update-ts (ts-now)
                            :text "do thing 1"
                            :is-completed t))
         (incomplete-todo-1
          (make-mxtodo-item :file-path "/path/to/notes/2021-7-10.md"
                            :file-line-number 11
                            :file-display-date-ts (make-ts :year 2021
                                                           :month 7
                                                           :day 10
                                                           :hour 0
                                                           :minute 0
                                                           :second 0)
                            :file-last-update-ts (ts-now)
                            :text "do thing 2"
                            :is-completed nil))
         (completed-todo-2
          (make-mxtodo-item :file-path "/path/to/notes/2021-7-11.md"
                            :file-line-number 10
                            :file-display-date-ts (make-ts :year 2021
                                                           :month 7
                                                           :day 11
                                                           :hour 0
                                                           :minute 0
                                                           :second 0)
                            :file-last-update-ts (ts-now)
                            :text "do thing 3"
                            :is-completed t))
         (incomplete-todo-2
          (make-mxtodo-item :file-path "/path/to/notes/2021-7-11.md"
                            :file-line-number 11
                            :file-display-date-ts (make-ts :year 2021
                                                           :month 7
                                                           :day 11
                                                           :hour 0
                                                           :minute 0
                                                           :second 0)
                            :file-last-update-ts (ts-now)
                            :text "do thing 4"
                            :is-completed nil))
         (todos (nshuffle (list completed-todo-1 incomplete-todo-1 completed-todo-2 incomplete-todo-2)))
         (expected (list incomplete-todo-2 incomplete-todo-1 completed-todo-1 completed-todo-2))
         (actual (mxtodo--sort-todos todos)))
    (should (equal expected actual))))

(defun make-test-notes-dir (&optional dir-prefix)
  "Create a temporary test directory for todo note files."
  (progn
    (unless dir-prefix
      (setq dir-prefix "notes"))
    (make-temp-file dir-prefix t)))

(defun random-date-str ()
  "Generate a random date string."
  (let ((day-of-month (1+ (random 30)))
        (month-of-year (1+ (random 12)))
        (year 2021))
    (format "%d-%02d-%02d" year month-of-year day-of-month)))

(defun make-todo-str (&optional text is-completed idx)
  "Make a test todo string."
  (progn
    (unless idx
      (setq idx (random)))
    (unless text
      (setq text (format "do thing %s" idx)))
    (unless is-completed
      (setq is-completed (equal (% (random 10) 2) 0)))
    (let ((checkbox (if is-completed "[x]" "[ ]")))
      (format "- %s %s" checkbox text))))

(defun make-test-notes-file (dir &optional num-todos date-str file-ext)
  "Create a temporary notes file for testing."
  (progn
    (unless date-str
      (setq date-str (random-date-str)))
    (unless file-ext
      (setq file-ext ".md"))
    (unless num-todos
      (setq num-todos 1))
    (let ((file-path (concat (file-name-as-directory dir) (format "%s%s" date-str file-ext))))
      (with-temp-file file-path
        (progn
          (dotimes (_ num-todos)
            (insert (concat (make-todo-str) "\n")))
          file-path)))))

(defun parse-todo-from-string (todo-line file-path file-line-number)
  "Read a single todo from a string."
  (if (string-match "^- \\[\\(x\\|[[:blank:]]\\)\\] \\(.*?\\)\\(?: (due \\(.*\\))\\)?$" todo-line)
      (let* ((file-display-date (file-name-base file-path))
             (completed-text (match-string 1 todo-line))
             (todo-text (match-string 2 todo-line))
             (is-completed (equal completed-text "x"))
             (due-date (mxtodo--ts-date-from-string (match-string 3 todo-line)))
             (todo-item
              (make-mxtodo-item
               :file-path file-path
               :file-line-number file-line-number
               :file-display-date-ts (mxtodo--ts-date-from-string file-display-date)
               :file-last-update-ts (mxtodo--file-last-modified file-path)
               :date-due-ts due-date
               :text todo-text
               :is-completed is-completed)))
        (cl-values todo-item nil))
    (cl-values nil "unable to parse")))

(defun read-test-notes-file (notes-file)
  "Read a NOTES-FILE and return a list of todo items ignoring empty lines."
  (-map-indexed
   (lambda
     (line-no line)
     (-let [(todo err) (parse-todo-from-string line notes-file (1+ line-no))]
       (if (not (equal err nil))
           (error err)
         todo)))
   (-filter
    (lambda (line) (not (string= line "")))
    (split-string (f-read-text notes-file) "\n"))))

(ert-deftest test-todo-is-fresh ()
  (let* ((notes-dir (make-test-notes-dir))
         (notes-file (make-test-notes-file notes-dir 1))
         (todos (read-test-notes-file notes-file))
         (todo (car todos))
         (expected t)
         (actual (mxtodo--todo-is-fresh-p todo)))
    (should (equal expected actual))))

(ert-deftest test-todo-is-stale ()
  (let* ((notes-dir (make-test-notes-dir))
         (notes-file (make-test-notes-file notes-dir 1))
         (todos (read-test-notes-file notes-file))
         (_ (set-file-times notes-file))
         (todo (car todos))
         (expected nil)
         (actual (mxtodo--todo-is-fresh-p todo)))
    (progn
      (set-file-times notes-file)
      (should (equal expected actual)))))

(ert-deftest test-creating-a-daily-notefile ()
  (let* ((notes-dir (make-test-notes-dir))
         (expected (concat (file-name-as-directory notes-dir) (mxtodo--render-date (ts-now)) mxtodo-file-extension))
         (actual (mxtodo-create-daily-note notes-dir)))
    (should (equal actual expected))))

(ert-deftest test-creating-a-todo-new-notefile ()
  (let* ((notes-dir (make-test-notes-dir))
         (todo-text "Water the garden")
         (due-date (ts-adjust 'day +7 (ts-now))))
    (should (not (equal (mxtodo--create-todo notes-dir nil todo-text due-date) nil)))))

(ert-deftest test-adding-a-todo-works-existing-notefile ()
  (let* ((notes-dir (make-test-notes-dir))
         (todo-text "Take out the garbage")
         (due-date (ts-adjust 'day +7 (ts-now))))
    (should (not (equal (mxtodo--create-todo notes-dir nil todo-text due-date) nil)))))

(ert-deftest test-search-directory-returns-the-expected-results ()
  (let* ((notes-dir (make-test-notes-dir))
         (notes-file (make-test-notes-file notes-dir 1))
         (actual (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]")))
    (progn
      (should (equal (length actual) 1))
      (should (equal notes-file (cdr (assoc "file_path" (elt actual 0))))))))

(ert-deftest test-search-directory-returns-wrong-type-error-if-dir-is-nil ()
  (should-error
   (mxtodo-searcher-search-directory nil ".md" "^- ?\\[[Xx ]\\]")
   :type 'wrong-type-argument))

(ert-deftest test-search-directory-returns-directory-not-found-if-dir-does-not-exist ()
  (let ((notes-dir "foo/bar/baz"))
    (should-error
     (actual (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]"))
     :type 'directory-not-found)))

(ert-deftest test-search-directory-returns-path-error-if-dir-param-is-file ()
  (let ((tmp-file (make-temp-file "notes")))
    (should-error
     (actual (mxtodo-searcher-search-directory tmp-file ".md" "^- ?\\[[Xx ]\\]"))
     :type 'path-is-not-a-directory)))
