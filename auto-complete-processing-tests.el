;;; auto-complete-processing-tests.el --- ERT tests for auto-complete-processing.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Rolando Pereira

;; Author: Rolando Pereira <finalyugi@sapo.pt>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A test suite for auto-complete-processing.el package

;;; Code:

(require 'ert)
(require 'auto-complete-processing)


(ert-deftest auto-complete-processing-test--no-repeated-functions ()
  "Check that there are no repeated functions in `auto-complete-processing--auto-complete-data'."
  (let ((count-functions (make-hash-table :test #'equal))
         (not-unique-functions '())     ; List containing functions that appear more than once
         )
    (mapc (lambda (data)
            (let* ((function-name (car data))
                    (current-count (gethash function-name count-functions 0)))
              (puthash function-name (1+ current-count) count-functions)))
      auto-complete-processing--auto-complete-data)
    (maphash (lambda (key value)        ; key = function-name and value = count
               (when (> value 1)
                 (push key not-unique-functions)))
      count-functions)
    (should (null not-unique-functions))))


(provide 'auto-complete-processing-tests)
;;; auto-complete-processing-tests.el ends here
