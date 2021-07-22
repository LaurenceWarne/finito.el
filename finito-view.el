;;; finito-view.el --- Graphical parts of finito -*- lexical-binding: t -*-

;; Copyright (C) 2021 Laurence Warne

;; Author: Laurence Warne

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
;; This file contains transients used by finito.

;;; Code:

(require 'dash)
(require 's)
(require 'transient)

;;; Transients

(defclass finito--transient-argument (transient-argument)
  ((plist-key :initarg :plist-key)))

(cl-defmethod transient-format-value ((obj finito--transient-argument))
  (let ((value (oref obj value)))
    (propertize (concat (oref obj argument)
                        (if (listp value)
                            (mapconcat #'identity value ",")
                          value))
                'face (if value
                          'transient-value
                        'transient-inactive-value))))

;; We always want a value to show up in ARGS for a transient arg function, so
;; we return the empty string instead of nil, since nil implies an unspecified
;; value
(cl-defmethod transient-infix-value ((obj finito--transient-argument))
  "Return the value of OBJ's `value' slot."
  (or (oref obj value) ""))

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
  :argument "max results=")

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
  :choices '("DateAdded" "Author" "Title"))

;;; Prefixes

;; TODO change name to finito and add autoloads?
(transient-define-prefix finito-dispatch ()
  "Search for books."
  ["Actions"
   ("m" "My Books"          finito-my-books)
   ("r" "Currently Reading" finito-open-currently-reading-collection)
   ("s" "Search"            finito-search)
   ("c" "Collections"       finito-collection)])

(transient-define-prefix finito-search ()
  "Search for books."
  :incompatible '(("isbn=" "author=") ("isbn=" "title=") ("isbn=" "max results="))
  ["By Keywords"
   (finito--title-arg :description "Title" :prompt "Title: ")
   (finito--author-arg :description "Author" :prompt "Author: ")
   (finito--max-results-arg :description "Max Results" :prompt "Max results: ")]
  ["Direct Lookup"
   (finito--isbn-arg :description "ISBN" :prompt "ISBN: ")]
  ["Actions"
   ;("c" "Copy Curl"     finito-search-request)
   ("s" "Search"        finito-search-request)])

(transient-define-prefix finito-collection ()
  "Search for books."
  ["Actions"
   ;("c" "Copy Curl"     finito-request)
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
    :prompt "Sort Books By: ")]
  ["Actions"
   ("u" "Update" finito-update-collection-request)])

;;; Misc functions

(defun finito--transient-args-plist (prefix)
  "Return the infixes of PREFIX as a plist."
  (-flatten-n 1 (--map (list (oref it plist-key) (oref it value))
                       (-filter #'finito--transient-argument-p
                                (transient-suffixes prefix)))))

(provide 'finito-view)
;;; finito-view.el ends here
