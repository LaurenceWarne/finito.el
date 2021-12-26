;;; finito-view.el --- Graphical parts of finito -*- lexical-binding: t -*-

;; Copyright (C) 2021 Laurence Warne

;; Author: Laurence Warne

;; Local variables:
;; package-lint-main-file: "finito.el"
;; end:

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

;; This file contains transients used by finito.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 's)
(require 'subr-x)
(require 'transient)

(require 'finito-core)

;;; Custom variables

(defcustom finito-save-last-search
  t
  "If non-nil, save the arguments of the last `finito-search'."
  :group 'finito
  :type 'boolean)

;;; Internal variables

(defvar finito--sort-asc-alist '(("ascending" . true) ("descending" . false)))

;;; Transients

(defclass finito--transient-argument (transient-argument)
  ((plist-key :initarg :plist-key)))

(cl-defmethod transient-format-value ((obj finito--transient-argument))
  "Format OBJ and return the result."
  (let ((value (oref obj value)))
    (propertize (if (listp value) (mapconcat #'identity value ",")
                  value)
                'face (if value
                          'transient-value
                        'transient-inactive-value))))

(cl-defmethod transient-init-value ((obj finito--transient-argument))
  "Initialize the value of OBJ using the value of the current prefix."
  (let* ((prefix-plist (oref transient--prefix value))
         (val-or-nil (plist-get prefix-plist (oref obj plist-key))))
      (oset obj value val-or-nil)))

(cl-defmethod transient-infix-set :around
  ((obj finito--transient-argument) value)
  "Unset incompatible infix arguments."
  (let ((arg (if (slot-boundp obj 'argument)
                 (oref obj argument)
               (oref obj argument-regexp))))
    (if-let* ((sic (and value arg transient--unset-incompatible))
              (spec (oref transient--prefix incompatible))
              (incomp-matching (cl-remove-if-not
                                (lambda (elt) (member arg elt)) spec))
              (incomp (flatten-list
                       (mapcar (lambda (e) (remove arg e)) incomp-matching))))
        (progn
          (cl-call-next-method obj value)
          (dolist (arg incomp)
            (when-let ((obj (cl-find-if (lambda (obj)
                                          (and (slot-boundp obj 'argument)
                                               (equal (oref obj argument) arg)))
                                        transient--suffixes)))
              (let ((transient--unset-incompatible nil))
                (transient-infix-set obj nil)))))
      (cl-call-next-method obj value))))

(defclass finito--search-prefix (transient-prefix) nil)

(cl-defmethod transient--history-push ((obj finito--search-prefix))
  "Push the current value of OBJ to its entry in `transient-history'."
  (let ((key (transient--history-key obj)))
    (setf (alist-get key transient-history)
          (let ((args (finito--transient-args-plist (oref obj command))))
            (cons args (delete args (alist-get key transient-history)))))))

(cl-defmethod transient-init-value :after ((obj finito--search-prefix))
  "Set the value of OBJ from history if applicable.

If OBJ has an empty value and `finito-save-last-search' is non-nil, switch
to the last value used for OBJ."
  (transient--history-init obj)
  (when (and finito-save-last-search
             (not (oref obj value))
             (cdr (oref obj history)))
    (let ((transient--prefix obj))
      (transient-history-prev))))

;;; Infix Arguments

(transient-define-argument finito--title-arg ()
  :class 'finito--transient-argument
  :key "t"
  :plist-key ':title
  :argument "title=")

(transient-define-argument finito--author-arg ()
  :class 'finito--transient-argument
  :key "a"
  :plist-key ':author
  :argument "author=")

(transient-define-argument finito--isbn-arg ()
  :class 'finito--transient-argument
  :key "i"
  :plist-key ':isbn
  :argument "isbn=")

(transient-define-argument finito--max-results-arg ()
  :class 'finito--transient-argument
  :key "n"
  :plist-key ':max-results
  :argument "max results="
  :reader #'transient-read-number-N+)

(transient-define-argument finito--collection-name-arg ()
  :class 'finito--transient-argument
  :key "c"
  :plist-key ':name
  :argument "name=")

(transient-define-argument finito--new-collection-name-arg ()
  :class 'finito--transient-argument
  :key "n"
  :plist-key ':new-name
  :argument "new name=")

(transient-define-argument finito--sort-arg ()
  :class 'finito--transient-argument
  :key "s"
  :plist-key ':sort
  :argument "Sort="
  :choices '("Date Added" "Author" "Title"))

(transient-define-argument finito--sort-asc ()
  :class 'finito--transient-argument
  :key "o"
  :plist-key ':sort-ascending
  :argument "Sort Ascending="
  :choices (-map #'car finito--sort-asc-alist))

;;; Prefixes

;;;###autoload (autoload 'finito "finito-view" nil t)
(transient-define-prefix finito ()
  "Search for books."
  ["Actions"
   ("m" "My Books"           finito-open-my-books-collection)
   ("r" "Currently Reading"  finito-open-currently-reading-collection)
   ("s" "Search"             finito-search)
   ("o" "Open a Collection"  finito-open-collection)
   ("c" "Collection Actions" finito-collection)])

(transient-define-prefix finito-search ()
  "Search for books."
  :class 'finito--search-prefix
  :incompatible '(("isbn=" "author=") ("isbn=" "title=") ("isbn=" "max results="))
  ["By Keywords"
   (finito--title-arg :description "Title" :prompt "Title: ")
   (finito--author-arg :description "Author" :prompt "Author: ")
   (finito--max-results-arg :description "Max Results" :prompt "Max results: ")]
  ["Direct Lookup"
   (finito--isbn-arg :description "ISBN" :prompt "ISBN: ")]
  ["Actions"
   ("C" "Copy Curl"     finito-search-request-curl-dbg)
   ("s" "Search"        finito-search-request)])

(transient-define-prefix finito-collection ()
  "Search for books."
  ["Actions"
   ("n" "Create a new Collection" finito-create-collection)
   ("o" "Open a Collection"       finito-open-collection)
   ("u" "Update a Collection"     finito-update-collection)
   ("d" "Delete a Collection"     finito-delete-collection)
   ("i" "Import a Collection"     ignore)
   ("e" "Export a Collection"     ignore)])

(transient-define-prefix finito-update-collection ()
  "Search for books."
  ["Attributes"
   ;; TODO read this using `finito--select-collection'
   (finito--collection-name-arg
    :description "Name"
    :prompt "Name: ")
   (finito--new-collection-name-arg
    :description "New Name"
    :prompt "New name: ")
   (finito--sort-arg
    :description "Sort Books By"
    :prompt "Sort Books By: ")
   (finito--sort-asc
    :description "Sort Ascending"
    :prompt "Ascending: ")]
  ["Actions"
   ("u" "Update" finito-update-collection-request)])

;;; Misc functions

(defun finito--transient-args-plist (prefix)
  "Return the infixes of PREFIX as a plist."
  (-flatten-n 1 (--map (list (oref it plist-key)
                             (oref it value))
                       (-filter #'finito--transient-argument-p
                                (transient-suffixes prefix)))))

(provide 'finito-view)
;;; finito-view.el ends here
