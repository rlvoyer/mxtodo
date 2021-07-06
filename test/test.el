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
