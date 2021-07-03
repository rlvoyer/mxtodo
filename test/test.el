;;; -*- lexical-binding: t -*-

(require 'ert)
(require 'ts)

(require 'todo-search)

(ert-deftest test-display-date-from-file-path ()
  (should (equal (todo-search--display-date-from-file-path "/foo/bar/2021-5-24.md")
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
    (should (equal (todo-search--render-date date)
                   "2021-5-24"))))

(ert-deftest test-make-todo-from-temp-file-line ()
  (let ((expected
         (make-todo-search-item :file-path "/Users/robertvoyer/Documents/Notes/2021-6-24.md"
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
         (todo-search--make-todo-from-temp-file-line  "/Users/robertvoyer/Documents/Notes/2021-6-24.md	10	- [x] write some unit tests	1624637870")))
    (should (equal expected actual))))

(defun todo-text-no-properties (rendered-todo-item)
  (substring-no-properties rendered-todo-item 0 (next-single-property-change 0 'invisible rendered-todo-item)))

(ert-deftest test-render-date ()
  (let* ((date
          (make-ts :year 2021
                   :month 6
                   :day 24
                   :hour 0
                   :minute 0
                   :second 0)) 
         (expected "2021-6-24")
         (actual (todo-search--render-date date)))    
    (should (equal expected actual))))

(ert-deftest test-render-todo ()
  (let* ((input-todo
          (make-todo-search-item :file-path "/Users/robertvoyer/Documents/Notes/2021-6-24.md"
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
         (actual (todo-text-no-properties (todo-search--render-todo input-todo))))
    (should (equal expected actual))))


(ert-deftest test-todo-completed-from-todo-text ()
  (should (equal (todo-search-todo-completed-p "- [x] buy groceries") t)))

;; (ert-deftest test-todo-toggle-changes-incomplete-to-complete ()
;;   (let* ((actual (make-todo-search-item))
;;          (expected (copy-todo-search-item actual)))
;;     (setf (todo-search-item-is-completed expected) t)
;;     (should (equal expected (todo-search--toggle-todo-completed actual)))))
