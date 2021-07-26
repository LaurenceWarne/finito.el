;;; finito-view.el --- http utilities for finito -*- lexical-binding: t -*-

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
;; This file contains http utilities used by finito.

;;; Code:

(require 's)

(require 'finito-graphql)

(defconst finito--headers
  '(("Content-Type" . "application/json")
    ("Accept" . "application/json")))

;;; Misc functions

(defun finito--search-request-plist
    (title-keywords author-keywords &optional max-results)
  "Return a plist with headers and body suitable for a search query.

The body will be deduced from TITLE-KEYWORDS, AUTHOR-KEYWORDS and MAX-RESULTS."
  (let* ((query-variable-str
          (format finito--search-query-variables
                  (if (> (length title-keywords) 0)
                      (s-wrap title-keywords "\"") "null")
                  (if (> (length author-keywords) 0)
                      (s-wrap  author-keywords "\"") "null")
                  (or max-results "null"))))
    `(:headers ,finito--headers
      :data
      ,(format "{\"query\":\"%s\", \"variables\": %s\}"
               finito--search-query
               query-variable-str))))

(defun finito--isbn-request-plist (isbn)
  "Return a plist with headers and body suitable for an isbn request.

ISBN should be isbn of the book to query for."
  `(:headers ,finito--headers
    :data
    ,(format "{\"query\":\"%s\", \"variables\": %s\}"
             finito--isbn-query
             (format finito--isbn-query-variables isbn))))

(defun finito--collection-request-plist (name)
  "Return a plist with headers and body suitable for a collection request.

NAME should be the name of the collection to query for."
  `(:headers ,finito--headers
    :data
    ,(format "{\"query\":\"%s\", \"variables\": %s\}"
             finito--collection-query
             (format finito--collection-query-variables name))))

(defun finito--collections-request-plist ()
  "Return a plist with headers and body suitable for a collections query."
  `(:headers ,finito--headers
    :data
    ,(format "{\"query\":\"%s\"}" finito--collections-query)))

(defun finito--create-collection-request-plist (name)
  "Return a plist with headers and body suitable for a collection request.

NAME should be the name of the collection to create."
  `(:headers ,finito--headers
    :data
    ,(format "{\"query\":\"%s\", \"variables\": %s\}"
             finito--create-collection-mutation
             (format finito--create-collection-mutation-variables name))))

(defun finito--delete-collection-request-plist (name)
  "Return a plist with headers and body suitable for a delete request.

NAME should be the name of the collection to delete."
  `(:headers ,finito--headers
    :data
    ,(format "{\"query\":\"%s\", \"variables\": %s\}"
             finito--delete-collection-mutation
             (format finito--delete-collection-mutation-variables name))))

(defun finito--update-collection-request-plist
    (current-name &optional new-name preferred-sort)
  "Return a plist with headers and body suitable for an update request.

CURRENT-NAME should be the name of the collection to update, NEW-NAME
should be the new name and PREFERRED-SORT the preferred sorting method.
All arguments should be strings."
  `(:headers ,finito--headers
    :data
    ,(format "{\"query\":\"%s\", \"variables\": %s\}"
             finito--update-collection-mutation
             (format finito--update-collection-mutation-variables
                     current-name
                     (if new-name (s-wrap new-name "\"") "null")
                     (or preferred-sort "null")))))

(defun finito--add-book-request-plist (name book)
  "Return a plist with headers and body for an add to collection request.

NAME should be the name of the collection, and BOOK should be the book (as an
alist) to add to it."
  `(:headers ,finito--headers
    :data ,(format "{\"query\":\"%s\", \"variables\": %s\}"
             finito--add-book-mutation
             (let-alist book
               (format
                finito--add-book-mutation-variables
                name
                (s-replace "\"" "'" .title)
                (finito--seq-to-json-list .authors)
                (s-replace "\"" "'" .description)
                (s-replace "\"" "'" .isbn)
                (s-replace "\"" "'" .img-uri))))))

(defun finito--remove-book-request-plist (collection isbn)
  "Return a plist with headers and body for a remove book request.

COLLECTION should be the name of the collection, and ISBN should be the isbn
of the book to remove."
  `(:headers ,finito--headers
    :data ,(format "{\"query\":\"%s\", \"variables\": %s\}"
                   finito--remove-book-mutation
                   (format finito--remove-book-mutation-variables
                           collection
                           isbn))))

(defun finito--rate-book-request-plist (book rating)
  "Return a plist with headers and body for a rating request.

BOOK should be the book (as an alist) to rate and RATING the rating."
  `(:headers ,finito--headers
    :data ,(format "{\"query\":\"%s\", \"variables\": %s\}"
                   finito--rate-book-mutation
                   (let-alist book
                     (format
                      finito--rate-book-mutation-variables
                      rating
                      (s-replace "\"" "'" .title)
                      (finito--seq-to-json-list .authors)
                      (s-replace "\"" "'" .description)
                      (s-replace "\"" "'" .isbn)
                      (s-replace "\"" "'" .img-uri))))))

(defun finito--start-reading-request-plist (book &optional start-date)
  "Return a plist with headers and body for a start reading request.

BOOK should be the book (as an alist) to start reading and START-DATE is an
optional start date which should be used if this book was started in the past."
  `(:headers ,finito--headers
    :data ,(format "{\"query\":\"%s\", \"variables\": %s\}"
                   finito--start-reading-mutation
                   (let-alist book
                     (format
                      finito--start-reading-mutation-variables
                      (if start-date (s-wrap start-date "\"") "null")
                      (s-replace "\"" "'" .title)
                      (finito--seq-to-json-list .authors)
                      (s-replace "\"" "'" .description)
                      (s-replace "\"" "'" .isbn)
                      (s-replace "\"" "'" .img-uri))))))

(defun finito--finish-reading-request-plist (book &optional date)
  "Return a plist with headers and body for a finish reading request.

BOOK should be the book (as an alist) to finish reading and DATE is an
optional date which should be used if this book was finished in the past."
  `(:headers ,finito--headers
    :data ,(format "{\"query\":\"%s\", \"variables\": %s\}"
                   finito--finish-reading-mutation
                   (let-alist book
                     (format
                      finito--finish-reading-mutation-variables
                      (if date (s-wrap date "\"") "null")
                      (s-replace "\"" "'" .title)
                      (finito--seq-to-json-list .authors)
                      (s-replace "\"" "'" .description)
                      (s-replace "\"" "'" .isbn)
                      (s-replace "\"" "'" .img-uri))))))

(defun finito--delete-book-data-request-plist (isbn)
  "Return a plist with headers and body for a delete book data request.

ISBN should be the isbn of the book to remove data for."
  `(:headers ,finito--headers
    :data ,(format "{\"query\":\"%s\", \"variables\": %s\}"
                   finito--delete-book-data-mutation
                   (format finito--delete-book-data-mutation-variables
                           isbn))))

(defun finito--seq-to-json-list (seq)
  "Return SEQ as an escaped json list."
  (concat "[" (mapconcat (##format "\"%s\"" %1) seq ",") "]"))

(provide 'finito-request)
;;; finito-request.el ends here
