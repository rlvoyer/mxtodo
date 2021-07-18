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
  "Tests the construction of a todo display date from its filename."
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

(ert-deftest test-extract-info-from-text-returns-due-date ()
  "Test that mxtodo--extract-info-from-text optionally returns due date."
  (let* ((actual (mxtodo--extract-info-from-text "- [ ] write more tests (due 2021-7-14)"))
         (expected-due-date
          (make-ts
           :year 2021
           :month 7
           :day 14
           :hour 0
           :minute 0
           :second 0))
         (expected (vector "write more tests" nil expected-due-date)))
    (should (equal (aref expected 2) (aref actual 2)))))

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
  "Test helper function that renders a RENDERED-TODO-ITEM as a string with no properties."
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
