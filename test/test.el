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

(defun setup-test-database ()
  "Set up a temporary test database and return its path."
  (let ((test-db-path (make-temp-file "mxtodo-test-db" nil ".db")))
    (setq mxtodo--db-path test-db-path)
    (setq mxtodo--db nil)  ; Force re-initialization
    test-db-path))

(defun teardown-test-database ()
  "Clean up the test database."
  (when mxtodo--db
    (sqlite-close mxtodo--db)
    (setq mxtodo--db nil))
  (when mxtodo--db-path
    (when (file-exists-p mxtodo--db-path)
      (delete-file mxtodo--db-path))))

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
               :text-hash (mxtodo--compute-text-hash todo-text)
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
         (todo (car todos))
         (expected nil))
    (progn
      (sit-for 1)
      (set-file-times notes-file)
      (should (equal expected (mxtodo--todo-is-fresh-p todo))))))

(ert-deftest test-creating-a-daily-notefile ()
  (let* ((notes-dir (make-test-notes-dir))
         (expected (concat (file-name-as-directory notes-dir) (mxtodo--render-date (ts-now)) mxtodo-file-extension))
         (actual (mxtodo-create-daily-note notes-dir)))
    (should (equal actual expected))))

(ert-deftest test-creating-a-todo-new-notefile ()
  (setup-test-database)
  (unwind-protect
      (let* ((notes-dir (make-test-notes-dir))
             (todo-text "Water the garden")
             (due-date (ts-adjust 'day +7 (ts-now))))
        (should (not (equal (mxtodo--create-todo notes-dir nil todo-text due-date) nil))))
    (teardown-test-database)))

(ert-deftest test-adding-a-todo-works-existing-notefile ()
  (setup-test-database)
  (unwind-protect
      (let* ((notes-dir (make-test-notes-dir))
             (todo-text "Take out the garbage")
             (due-date (ts-adjust 'day +7 (ts-now))))
        (should (not (equal (mxtodo--create-todo notes-dir nil todo-text due-date) nil))))
    (teardown-test-database)))

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
     (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]")
     :type 'directory-not-found)))

(ert-deftest test-search-directory-returns-path-error-if-dir-param-is-file ()
  (let ((tmp-file (make-temp-file "notes")))
    (should-error
     (mxtodo-searcher-search-directory tmp-file ".md" "^- ?\\[[Xx ]\\]")
     :type 'path-is-not-a-directory)))

(ert-deftest test-trim-system-info-linux ()
  (should (equal (mxtodo--trim-system-info "x86_64-pc-linux-gnu") "x86_64-unknown-linux-gnu")))

(ert-deftest test-trim-system-info-x86-mac ()
  (should (equal (mxtodo--trim-system-info "x86_64-apple-darwin18.7.0") "x86_64-apple-darwin")))

(ert-deftest test-lib-extension-linux ()
  (should (equal (mxtodo--lib-extension "x86_64-pc-linux-gnu") "so")))

(ert-deftest test-lib-extension-mac ()
  (should (equal (mxtodo--lib-extension "aarch64-apple-darwin21.6.0") "dylib")))

(ert-deftest test-due-date-preserved-when-todos-reordered ()
  "Test that due dates stay with the correct TODO when TODOs are reordered in a file."
  (let* ((_ (setup-test-database))
         (notes-dir (make-test-notes-dir))
         (date-str "2021-01-15")
         (notes-file (concat (file-name-as-directory notes-dir) date-str ".md"))
         (expected-due-date (make-ts :year 2021 :month 7 :day 1 :hour 0 :minute 0 :second 0)))
    (unwind-protect
        (progn
          ;; Create initial file with 3 TODOs
          (with-temp-file notes-file
            (insert "- [ ] Task A\n")
            (insert "- [ ] Task B\n")
            (insert "- [ ] Task C\n"))

          ;; Initial scan - process all TODOs
          (let* ((raw-todos (append (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]") nil))
                 (todos (mapcar (lambda (v) (mxtodo--make-todo-with-reconciliation v)) raw-todos)))

            ;; Verify we have 3 TODOs
            (should (equal (length todos) 3))

            ;; Find "Task B" and set a due date on it
            (let* ((task-b (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task B")) todos))
                   (task-b-file-path (mxtodo-item-file-path task-b))
                   (task-b-line-number (mxtodo-item-file-line-number task-b))
                   (task-b-text-hash (mxtodo-item-text-hash task-b)))

              (should (not (null task-b)))
              (should (equal task-b-line-number 2))  ; Task B is at line 2 initially

              ;; Set due date on Task B
              (mxtodo-db--update task-b-file-path
                                 task-b-line-number
                                 task-b-text-hash
                                 (truncate (ts-unix expected-due-date))
                                 nil
                                 '()))

            ;; Now reorder the file: move Task B to line 1
            (with-temp-file notes-file
              (insert "- [ ] Task B\n")
              (insert "- [ ] Task A\n")
              (insert "- [ ] Task C\n"))

            ;; Re-scan the file
            (let* ((raw-todos-2 (append (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]") nil))
                   (todos-2 (mapcar (lambda (v) (mxtodo--make-todo-with-reconciliation v)) raw-todos-2)))

              ;; Verify we still have 3 TODOs
              (should (equal (length todos-2) 3))

              ;; Find Task A and Task B
              (let ((task-a (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task A")) todos-2))
                    (task-b (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task B")) todos-2)))

                ;; Task B should now be at line 1
                (should (equal (mxtodo-item-file-line-number task-b) 1))

                ;; Task A should now be at line 2
                (should (equal (mxtodo-item-file-line-number task-a) 2))

                ;; The critical assertion: Task B should still have the due date
                (should (not (null (mxtodo-item-date-due-ts task-b))))
                (should (ts= (mxtodo-item-date-due-ts task-b) expected-due-date))

                ;; Task A should NOT have a due date
                (should (null (mxtodo-item-date-due-ts task-a)))))))

      ;; Cleanup
      (teardown-test-database))))

;;; Top-3 TODOs Tests

(ert-deftest test-mark-todo-as-top-three ()
  "Test marking a TODO as one of the top 3."
  (let* ((_ (setup-test-database))
         (notes-dir (make-test-notes-dir))
         (notes-file (make-test-notes-file notes-dir 1))
         (todos (read-test-notes-file notes-file))
         (todo (car todos))
         (file-path (mxtodo-item-file-path todo))
         (line-number (mxtodo-item-file-line-number todo))
         (text-hash (mxtodo-item-text-hash todo))
         (current-time (truncate (ts-unix (ts-now)))))
    (unwind-protect
        (progn
          ;; First insert the TODO into the database
          (mxtodo-db--insert file-path line-number text-hash nil nil '())

          ;; Mark it as top-3
          (mxtodo-db--set-top-three file-path line-number current-time)

          ;; Retrieve and verify
          (let ((retrieved (mxtodo-db--get-todo file-path line-number)))
            (should (equal (nth 9 retrieved) 1))
            (should (equal (nth 10 retrieved) current-time))))
      (teardown-test-database))))

(ert-deftest test-unmark-todo-from-top-three ()
  "Test unmarking a TODO from the top 3."
  (let* ((_ (setup-test-database))
         (notes-dir (make-test-notes-dir))
         (notes-file (make-test-notes-file notes-dir 1))
         (todos (read-test-notes-file notes-file))
         (todo (car todos))
         (file-path (mxtodo-item-file-path todo))
         (line-number (mxtodo-item-file-line-number todo))
         (text-hash (mxtodo-item-text-hash todo))
         (current-time (truncate (ts-unix (ts-now)))))
    (unwind-protect
        (progn
          ;; Insert and mark as top-3
          (mxtodo-db--insert file-path line-number text-hash nil nil '())
          (mxtodo-db--set-top-three file-path line-number current-time)

          ;; Unmark it
          (mxtodo-db--unset-top-three file-path line-number)

          ;; Verify it's no longer top-3
          (let ((retrieved (mxtodo-db--get-todo file-path line-number)))
            (should (equal (nth 9 retrieved) 0))
            (should (null (nth 10 retrieved)))))
      (teardown-test-database))))

(ert-deftest test-get-top-three-todos ()
  "Test retrieving the list of top 3 TODOs."
  (let* ((_ (setup-test-database))
         (notes-dir (make-test-notes-dir))
         (notes-file (concat (file-name-as-directory notes-dir) "2021-01-15.md")))
    (unwind-protect
        (progn
          ;; Create a file with 5 TODOs
          (with-temp-file notes-file
            (insert "- [ ] Task 1\n")
            (insert "- [ ] Task 2\n")
            (insert "- [ ] Task 3\n")
            (insert "- [ ] Task 4\n")
            (insert "- [ ] Task 5\n"))

          ;; Scan and insert into database
          (let* ((raw-todos (append (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]") nil))
                 (todos (mapcar (lambda (v) (mxtodo--make-todo-with-reconciliation v)) raw-todos)))

            ;; Mark 3 of them as top-3 with different timestamps
            (let ((task-1 (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task 1")) todos))
                  (task-3 (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task 3")) todos))
                  (task-5 (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task 5")) todos)))

              (mxtodo-db--set-top-three (mxtodo-item-file-path task-1)
                                        (mxtodo-item-file-line-number task-1)
                                        1000)
              (mxtodo-db--set-top-three (mxtodo-item-file-path task-3)
                                        (mxtodo-item-file-line-number task-3)
                                        2000)
              (mxtodo-db--set-top-three (mxtodo-item-file-path task-5)
                                        (mxtodo-item-file-line-number task-5)
                                        3000)

              ;; Get top-3 list
              (let ((top-three (mxtodo-db--get-top-three-todos)))
                (should (equal (length top-three) 3))

                ;; Verify the correct TODOs are returned (should be sorted by timestamp)
                ;; Column 2 is line_number
                (should (equal (nth 2 (nth 0 top-three)) 1))  ; Task 1
                (should (equal (nth 2 (nth 1 top-three)) 3))  ; Task 3
                (should (equal (nth 2 (nth 2 top-three)) 5)))))) ; Task 5
      (teardown-test-database))))

(ert-deftest test-top-three-limit-enforced ()
  "Test that marking a 4th TODO removes the oldest one (FIFO)."
  (let* ((_ (setup-test-database))
         (notes-dir (make-test-notes-dir))
         (notes-file (concat (file-name-as-directory notes-dir) "2021-01-15.md")))
    (unwind-protect
        (progn
          ;; Create a file with 4 TODOs
          (with-temp-file notes-file
            (insert "- [ ] Task 1\n")
            (insert "- [ ] Task 2\n")
            (insert "- [ ] Task 3\n")
            (insert "- [ ] Task 4\n"))

          ;; Scan and insert into database
          (let* ((raw-todos (append (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]") nil))
                 (todos (mapcar (lambda (v) (mxtodo--make-todo-with-reconciliation v)) raw-todos)))

            ;; Mark all 4 sequentially with increasing timestamps
            (let ((task-1 (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task 1")) todos))
                  (task-2 (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task 2")) todos))
                  (task-3 (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task 3")) todos))
                  (task-4 (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task 4")) todos)))

              ;; Mark first 3
              (mxtodo-db--set-top-three (mxtodo-item-file-path task-1)
                                        (mxtodo-item-file-line-number task-1)
                                        1000)
              (mxtodo-db--set-top-three (mxtodo-item-file-path task-2)
                                        (mxtodo-item-file-line-number task-2)
                                        2000)
              (mxtodo-db--set-top-three (mxtodo-item-file-path task-3)
                                        (mxtodo-item-file-line-number task-3)
                                        3000)

              ;; Enforce the limit (should have 3)
              (mxtodo--enforce-top-three-limit)
              (should (equal (mxtodo-db--count-top-three) 3))

              ;; Mark the 4th TODO
              (mxtodo-db--set-top-three (mxtodo-item-file-path task-4)
                                        (mxtodo-item-file-line-number task-4)
                                        4000)

              ;; Enforce limit again - Task 1 (oldest) should be removed
              (mxtodo--enforce-top-three-limit)

              ;; Should still have exactly 3
              (should (equal (mxtodo-db--count-top-three) 3))

              ;; Verify Task 1 is no longer in top-3
              (let ((task-1-retrieved (mxtodo-db--get-todo (mxtodo-item-file-path task-1)
                                                           (mxtodo-item-file-line-number task-1))))
                (should (equal (nth 9 task-1-retrieved) 0)))

              ;; Verify Tasks 2, 3, 4 are still in top-3
              (let ((top-three (mxtodo-db--get-top-three-todos)))
                (should (equal (length top-three) 3))
                ;; Column 2 is line_number
                (should (cl-some (lambda (todo) (equal (nth 2 todo) 2)) top-three))
                (should (cl-some (lambda (todo) (equal (nth 2 todo) 3)) top-three))
                (should (cl-some (lambda (todo) (equal (nth 2 todo) 4)) top-three))))))
      (teardown-test-database))))

(ert-deftest test-top-three-cleared-on-completion ()
  "Test that completing a TODO automatically clears its top-3 status."
  (let* ((_ (setup-test-database))
         (notes-dir (make-test-notes-dir))
         (notes-file (make-test-notes-file notes-dir 1))
         (todos (read-test-notes-file notes-file))
         (todo (car todos))
         (file-path (mxtodo-item-file-path todo))
         (line-number (mxtodo-item-file-line-number todo))
         (text-hash (mxtodo-item-text-hash todo))
         (current-time (truncate (ts-unix (ts-now)))))
    (unwind-protect
        (progn
          ;; Insert and mark as top-3
          (mxtodo-db--insert file-path line-number text-hash nil nil '())
          (mxtodo-db--set-top-three file-path line-number current-time)

          ;; Verify it's marked
          (should (equal (nth 9 (mxtodo-db--get-todo file-path line-number)) 1))

          ;; Complete the TODO (update with completion date)
          (mxtodo-db--update file-path line-number text-hash nil current-time '())

          ;; Clear top-3 status on completion
          (mxtodo-db--clear-top-three-for-completed file-path line-number)

          ;; Verify it's no longer top-3
          (let ((retrieved (mxtodo-db--get-todo file-path line-number)))
            (should (equal (nth 9 retrieved) 0))))
      (teardown-test-database))))

(ert-deftest test-top-three-preserved-when-todos-reordered ()
  "Test that top-3 status stays with the correct TODO when TODOs are reordered."
  (let* ((_ (setup-test-database))
         (notes-dir (make-test-notes-dir))
         (date-str "2021-01-15")
         (notes-file (concat (file-name-as-directory notes-dir) date-str ".md")))
    (unwind-protect
        (progn
          ;; Create initial file with 3 TODOs
          (with-temp-file notes-file
            (insert "- [ ] Task A\n")
            (insert "- [ ] Task B\n")
            (insert "- [ ] Task C\n"))

          ;; Initial scan
          (let* ((raw-todos (append (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]") nil))
                 (todos (mapcar (lambda (v) (mxtodo--make-todo-with-reconciliation v)) raw-todos)))

            (should (equal (length todos) 3))

            ;; Mark Task B as top-3
            (let* ((task-b (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task B")) todos))
                   (task-b-file-path (mxtodo-item-file-path task-b))
                   (task-b-line-number (mxtodo-item-file-line-number task-b)))

              (should (not (null task-b)))
              (should (equal task-b-line-number 2))  ; Task B is at line 2 initially

              ;; Mark as top-3
              (mxtodo-db--set-top-three task-b-file-path task-b-line-number 1000))

            ;; Reorder: move Task B to line 1
            (with-temp-file notes-file
              (insert "- [ ] Task B\n")
              (insert "- [ ] Task A\n")
              (insert "- [ ] Task C\n"))

            ;; Re-scan
            (let* ((raw-todos-2 (append (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]") nil))
                   (todos-2 (mapcar (lambda (v) (mxtodo--make-todo-with-reconciliation v)) raw-todos-2)))

              (should (equal (length todos-2) 3))

              ;; Find Task B and Task A
              (let ((task-a (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task A")) todos-2))
                    (task-b (cl-find-if (lambda (todo) (string= (mxtodo-item-text todo) "Task B")) todos-2)))

                ;; Task B should now be at line 1
                (should (equal (mxtodo-item-file-line-number task-b) 1))

                ;; Task B should still be marked as top-3
                (should (equal (mxtodo-item-is-top-three task-b) t))

                ;; Task A should NOT be marked as top-3
                (should (null (mxtodo-item-is-top-three task-a)))))))

      (teardown-test-database))))

(ert-deftest test-render-top-three-todo ()
  "Test that a top-3 TODO renders with the background face."
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
                            :text "important task"
                            :is-completed nil
                            :is-top-three t
                            :top-three-marked-ts (make-ts :year 2021 :month 6 :day 24)))
         (rendered (mxtodo--render-todo input-todo))
         (face-start 0))
    ;; Verify the face is applied to the visible portion
    (should (equal (get-text-property face-start 'face rendered) 'mxtodo--top-three-face))))

(ert-deftest test-render-normal-todo-no-top-three ()
  "Test that a normal TODO does not have the top-3 background face."
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
                            :text "normal task"
                            :is-completed nil
                            :is-top-three nil))
         (rendered (mxtodo--render-todo input-todo))
         (face-start 0))
    ;; Verify no top-three face is applied
    (should (not (equal (get-text-property face-start 'face rendered) 'mxtodo--top-three-face)))))

(ert-deftest test-unique-constraint-when-todo-replaced-at-same-line ()
  "Test that replacing a TODO at the same line number doesn't violate UNIQUE constraint.
When a different TODO moves to occupy the same line number as an existing TODO,
the system should delete the old record before inserting the new one."
  (let* ((_ (setup-test-database))
         (notes-dir (make-test-notes-dir))
         (date-str "2021-01-15")
         (notes-file (concat (file-name-as-directory notes-dir) date-str ".md")))
    (unwind-protect
        (progn
          ;; Create initial file with 2 TODOs
          (with-temp-file notes-file
            (insert "- [ ] Task A\n")
            (insert "- [ ] Task B\n"))

          ;; Initial scan - process both TODOs
          (let* ((raw-todos (append (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]") nil))
                 (todos (mapcar (lambda (v) (mxtodo--make-todo-with-reconciliation v)) raw-todos)))

            ;; Verify we have 2 TODOs
            (should (equal (length todos) 2))

            ;; Now replace the file content: Task B moves to line 1, Task A is deleted
            (with-temp-file notes-file
              (insert "- [ ] Task B\n"))

            ;; Re-scan the file - this should NOT throw a UNIQUE constraint error
            ;; Without the fix, this would fail because it tries to insert Task B at line 1
            ;; while the old Task A record at line 1 still exists
            (let* ((raw-todos-2 (append (mxtodo-searcher-search-directory notes-dir ".md" "^- ?\\[[Xx ]\\]") nil))
                   (todos-2 (mapcar (lambda (v) (mxtodo--make-todo-with-reconciliation v)) raw-todos-2)))

              ;; Should succeed with only 1 TODO now
              (should (equal (length todos-2) 1))

              ;; Verify it's Task B at line 1
              (let ((task-b (car todos-2)))
                (should (equal (mxtodo-item-text task-b) "Task B"))
                (should (equal (mxtodo-item-file-line-number task-b) 1))))))

      ;; Cleanup
      (teardown-test-database))))
