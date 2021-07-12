;;; -*- lexical-binding: t -*-

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
  "Tests the construction of a TODO display date from its filename."
  (should (equal (mxtodo--display-date-from-file-path "/foo/bar/2021-5-24.md")
                 (make-ts 
                  :year 2021
                  :month 5
                  :day 24
                  :hour 0
                  :minute 0
                  :second 0))))

(ert-deftest test-render-a-todo-date ()
  "Tests that a TODO renders correctly."
  (let ((date (make-ts 
               :year 2021
               :month 5
               :day 24
               :hour 0
               :minute 0
               :second 0)))
    (should (equal (mxtodo--render-date date)
                   "2021-5-24"))))

(ert-deftest test-make-todo-from-temp-file-line ()
  "Tests that a TODO can be constructed from a temp file line."
  (let ((expected
         (make-mxtodo-item :file-path "/Users/robertvoyer/Documents/Notes/2021-6-24.md"
                                :file-line-number 10
                                :file-display-date-ts (make-ts :year 2021
                                                               :month 6
                                                               :day 24
                                                               :hour 0
                                                               :minute 0
                                                               :second 0)
                                :file-last-update-ts (make-ts :unix 1624637870)
                                :text "write some unit tests"
                                :is-completed t))
        (actual
         (mxtodo--make-todo-from-temp-file-line  "/Users/robertvoyer/Documents/Notes/2021-6-24.md	10	- [x] write some unit tests	1624637870")))
    (should (equal expected actual))))

(defun todo-text-no-properties (rendered-todo-item)
  "Test helper function that renders a TODO as a string with no properties."
  (substring-no-properties rendered-todo-item 0 (next-single-property-change 0 'invisible rendered-todo-item)))

(ert-deftest test-render-date ()
  "Tests that a TODO date renders correctly."
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

(ert-deftest test-render-todo ()
  "Tests that a TODO renders correctly."
  (let* ((input-todo
          (make-mxtodo-item :file-path "/Users/robertvoyer/Documents/Notes/2021-6-24.md"
                            :file-line-number 10
                            :file-display-date-ts (make-ts :year 2021
                                                           :month 6
                                                           :day 24
                                                           :hour 0
                                                           :minute 0
                                                           :second 0)
                            :file-last-update-ts (make-ts :unix 1624637870)
                            :text "write some unit tests"
                            :is-completed nil))
         (expected "- [ ] write some unit tests (created 2021-6-24 / )")
         (actual (todo-text-no-properties (mxtodo--render-todo input-todo))))
    (should (equal expected actual))))

(defun generate-todo-file (directory)
  "Generate a test TODO file."
  (let* ((year 2021)
         (month (1+ (random 12)))
         (day (1+ (random 31)))
         (date-str (format "%d-%d-%d" year month day))
         (filename (format "%s.md" date-str))
         (todo-file (concat (file-name-as-directory directory) filename)))
    (with-current-buffer (find-file-noselect todo-file t t)
      (dotimes (i (random 10))
        (let ((is-completed (equal (random 2) 1)))
          (insert (format "- [%s] do thing %d" (if is-completed "x" "") i "\n")))
        (insert "\n"))
      (save-buffer)
      (kill-buffer))))

(defun setup-test-data (directory)
  "Create a temp directory with `num-files` TODO files."
  (dotimes (i 5)
    (generate-todo-file directory)))

(defun read-todos-from-buffer (buffer-name)
  "Read each TODO from the current buffer and construct a mxtodo-item from each line, returning a list."
  (with-current-buffer buffer-name
    (save-excursion
      (let ((buffer-text (buffer-string))
            (todos (list)))
        (goto-char (point-min))
        (while (not (eobp))
          (let ((todo (mxtodo--read-todo-from-line)))
            (setq todos (cons todo todos)))
          (forward-line 1))
        (reverse todos)))))

(defun sort-todos (todos)
  "Sort the specified list of TODO items."
  (let ((sorted (cl-sort
                 (copy-tree todos)
                 'ts>
                 :key (lambda (x) (mxtodo-item-file-display-date-ts x)))))
    sorted))

(ert-deftest test-buffer-is-sorted-by-create-date ()
  "Tests that the TODO buffer is sorted by create date by default."
  (with-temp-buffer
    (setup-test-data (temporary-file-directory))
    (mxtodo-make-todo-buffer (current-buffer) (temporary-file-directory))
    (let* ((todos (read-todos-from-buffer (current-buffer)))
           (todos-sorted (sort-todos todos))
           (todos-display-dates (mapcar 'mxtodo-item-file-display-date-ts todos))
           (todos-display-dates-sorted (mapcar 'mxtodo-item-file-display-date-ts todos-sorted))
           (actual todos-display-dates)
           (expected todos-display-dates-sorted))
      (should (equal expected actual)))))

