;;; auto-complete-processing.el --- Auto-complete sources for processing  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Rolando Pereira

;; Author: Rolando Pereira <finalyugi@sapo.pt>
;; Keywords: convenience

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

;; 

;;; Code:

(defvar auto-complete-processing--auto-complete-data
  '("FloatList.add"
     "FloatList.append"
     "FloatList.array"
     "FloatList.clear"
     "FloatList.div"
     "FloatList.get"
     "FloatList.hasValue"
     "FloatList.max"
     "FloatList.min"
     "FloatList.mult"
     "FloatList.remove"
     "FloatList.reverse"
     "FloatList.set"
     "FloatList.shuffle"
     "FloatList.size"
     "FloatList.sort"
     "FloatList.sortReverse"
     "FloatList.sub"
     "IntList.add"
     "IntList.append"
     "IntList.array"
     "IntList.clear"
     "IntList.div"
     "IntList.get"
     "IntList.hasValue"
     "IntList.increment"
     "IntList.max"
     "IntList.min"
     "IntList.mult"
     "IntList.remove"
     "IntList.reverse"
     "IntList.set"
     "IntList.shuffle"
     "IntList.size"
     "IntList.sort"
     "IntList.sortReverse"
     "IntList.sub"
     "StringList.append"
     "StringList.array"
     "StringList.clear"
     "StringList.get"
     "StringList.hasValue"
     "StringList.lower"
     "StringList.remove"
     "StringList.reverse"
     "StringList.set"
     "StringList.shuffle"
     "StringList.size"
     "StringList.sort"
     "StringList.sortReverse"
     "StringList.upper")
  "List of functions available in auto-complete-processing")

(defvar auto-complete-processing--functions-to-remove-prefix
  '(("FloatList.add" . "add")
     ("FloatList.append" . "append")
     ("FloatList.array" . "array")
     ("FloatList.clear" . "clear")
     ("FloatList.div" . "div")
     ("FloatList.get" . "get")
     ("FloatList.hasValue" . "hasValue")
     ("FloatList.max" . "max")
     ("FloatList.min" . "min")
     ("FloatList.mult" . "mult")
     ("FloatList.remove" . "remove")
     ("FloatList.reverse" . "reverse")
     ("FloatList.set" . "set")
     ("FloatList.shuffle" . "shuffle")
     ("FloatList.size" . "size")
     ("FloatList.sort" . "sort")
     ("FloatList.sortReverse" . "sortReverse")
     ("FloatList.sub" . "sub")
     ("IntList.add" . "add")
     ("IntList.append" . "append")
     ("IntList.array" . "array")
     ("IntList.clear" . "clear")
     ("IntList.div" . "div")
     ("IntList.get" . "get")
     ("IntList.hasValue" . "hasValue")
     ("IntList.increment" . "increment")
     ("IntList.max" . "max")
     ("IntList.min" . "min")
     ("IntList.mult" . "mult")
     ("IntList.remove" . "remove")
     ("IntList.reverse" . "reverse")
     ("IntList.set" . "set")
     ("IntList.shuffle" . "shuffle")
     ("IntList.size" . "size")
     ("IntList.sort" . "sort")
     ("IntList.sortReverse" . "sortReverse")
     ("IntList.sub" . "sub")
     ("StringList.append" . "append")
     ("StringList.array" . "array")
     ("StringList.clear" . "clear")
     ("StringList.get" . "get")
     ("StringList.hasValue" . "hasValue")
     ("StringList.lower" . "lower")
     ("StringList.remove" . "remove")
     ("StringList.reverse" . "reverse")
     ("StringList.set" . "set")
     ("StringList.shuffle" . "shuffle")
     ("StringList.size" . "size")
     ("StringList.sort" . "sort")
     ("StringList.sortReverse" . "sortReverse")
     ("StringList.upper" . "upper")))

(defun auto-complete-processing--get-candidates ()
  "Return a list of strings containing the candidates to pass to auto-complete."
  auto-complete-processing--auto-complete-data)

(defun auto-complete-processing--remove-class-prefix-from-method ()
  "Convert things like \"FloadList.sortReverse()\" into \"sortReverse()\" if needed."
  ;; Check `ac-complete-1' to see how the variable `ac-last-completion' is created
  (let ((just-completed-string (substring-no-properties (cdr ac-last-completion))))
    (unless (looking-back just-completed-string)
      ;; We should never reach this form.
      (error "Not `looking-back' at `ac-last-completion'."))
    (save-excursion
      (re-search-backward just-completed-string)
      (let ((replace (assoc just-completed-string auto-complete-processing--functions-to-remove-prefix)))
        (when replace
          (replace-match (cdr replace))
          ;; If there is a ". " before `just-completed-string' then remove the space
          ;; This let's you do something like the following:
          ;;     FloatList foo;
          ;;     foo. |                <-- cursor is here
          ;;     foo. float|[list.add] <-- cursor needs to be separated from "foo." to start the auto-complete
          ;;     foo.add               <-- pressing RET joins the "add" with the "foo."
          (when (looking-back (concat "\\\. " (cdr replace)))
            (re-search-backward (concat "\\\. " (cdr replace)))
            ;; Remove the space after the "." character
            (replace-match (concat "." (cdr replace)) nil 'literal)))))))

(defvar ac-source-processing
  '((candidates . auto-complete-processing--get-candidates)
     (action . auto-complete-processing--remove-class-prefix-from-method)
     (cache)))



(provide 'auto-complete-processing)
;;; auto-complete-processing.el ends here
