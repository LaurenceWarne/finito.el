;;; finito-graphql.el --- GraphQL helpers -*- lexical-binding: t -*-

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

;; This file contains utilities for working with libro finito's GraphQL api

;;; Code:

(require 'graphql)

;;; Graphql queries and mutations

(defconst finito--search-query
  (graphql-query
   (:arguments
    (($titleKeywords . String)
     ($authorKeywords . String)
     ($maxResults . Int)
     ($langRestrict . String))
    (books
     :arguments ((titleKeywords . ($ titleKeywords))
                 (authorKeywords . ($ authorKeywords))
                 (maxResults . ($ maxResults))
                 (langRestrict . ($ langRestrict)))
     title authors description isbn thumbnailUri
     rating review startedReading lastRead))))

;; We don't add quotes around %s since null is a value that could potentially
;; be used, and we shouldn't quote that
(defconst finito--search-query-variables
  "{
     \"titleKeywords\": %s,
     \"authorKeywords\": %s,
     \"maxResults\": %s,
     \"langRestrict\": \"%s\"
   }")

(defconst finito--isbn-query
  (graphql-query
   (:arguments
    (($isbn . String!))
    (book
     :arguments ((isbn . ($ isbn)))
     title authors description isbn thumbnailUri
     rating review startedReading lastRead))))

(defconst finito--isbn-query-variables "{\"isbn\": \"%s\"}")

(defconst finito--collection-query
  (graphql-query
   (:arguments
    (($name . String!)
     ($booksPagination . PaginationInput))
    (collection
     :arguments ((name . ($ name))
                 (booksPagination . ($ booksPagination)))
     (books title authors description isbn thumbnailUri
            rating review startedReading lastRead)
     (pageInfo totalBooks)))))

(defconst finito--collection-query-variables
  "{
     \"name\": \"%s\",
     \"booksPagination\": %s
  }")

(defconst finito--collections-query
  (graphql-query
   (collections
    (nil name))))

(defconst finito--create-collection-mutation
  (graphql-mutation
   (:arguments
    (($name . String!))
    (createCollection
     :arguments ((name . ($ name)))
     name))))

(defconst finito--create-collection-mutation-variables "{\"name\": \"%s\"}")

(defconst finito--add-book-mutation
  (graphql-mutation
   (:arguments
    (($collection . String!)
     ($book . BookInput!))
    (addBook
     :arguments ((collection . ($ collection))
                 (book . ($ book)))
     name))))

(defconst finito--add-book-mutation-variables
  "{
     \"collection\": %s,
     \"book\": {
       \"title\": \"%s\",
       \"authors\": %s,
       \"description\": \"%s\",
       \"isbn\": \"%s\",
       \"thumbnailUri\": \"%s\"
     }
   }")

(defconst finito--delete-collection-mutation
  (graphql-mutation
   (:arguments
    (($name . String!))
    (deleteCollection
     :arguments ((name . ($ name)))))))

(defconst finito--delete-collection-mutation-variables "{\"name\": \"%s\"}")

(defconst finito--update-collection-mutation
  (graphql-mutation
   (:arguments
    (($currentName . String!)
     ($newName . String)
     ($preferredSortType . SortType)
     ($sortAscending . Boolean))
    (updateCollection
     :arguments ((currentName . ($ currentName))
                 (newName . ($ newName))
                 (preferredSortType . ($ preferredSortType))
                 (sortAscending . ($ sortAscending)))
     name))))

(defconst finito--update-collection-mutation-variables
  "{
     \"currentName\": \"%s\",
     \"newName\": %s,
     \"preferredSortType\": \"%s\",
     \"sortAscending\": %s
   }")

(defconst finito--remove-book-mutation
  (graphql-mutation
   (:arguments
    (($collection . String!)
     ($isbn . String!))
    (removeBook
     :arguments ((collection . ($ collection))
                 (isbn . ($ isbn)))))))

(defconst finito--remove-book-mutation-variables
  "{\"collection\": \"%s\", \"isbn\": \"%s\"}")

(defconst finito--rate-book-mutation
  (graphql-mutation
   (:arguments
    (($rating . Int!)
     ($book . BookInput!))
    (rateBook
     :arguments ((rating . ($ rating))
                 (book . ($ book)))
     title authors description isbn thumbnailUri
     rating review startedReading lastRead))))

(defconst finito--rate-book-mutation-variables
  "{
     \"rating\": %s,
     \"book\": {
       \"title\": \"%s\",
       \"authors\": %s,
       \"description\": \"%s\",
       \"isbn\": \"%s\",
       \"thumbnailUri\": \"%s\"
     }
   }")

(defconst finito--review-book-mutation
  (graphql-mutation
   (:arguments
    (($review . String!)
     ($book . BookInput!))
    (addBookReview
     :arguments ((review . ($ review))
                 (book . ($ book)))
     title authors description isbn thumbnailUri
     rating review startedReading lastRead))))

(defconst finito--review-book-mutation-variables
  "{
     \"review\": \"%s\",
     \"book\": {
       \"title\": \"%s\",
       \"authors\": %s,
       \"description\": \"%s\",
       \"isbn\": \"%s\",
       \"thumbnailUri\": \"%s\"
     }
   }")

(defconst finito--start-reading-mutation
  (graphql-mutation
   (:arguments
    (($date . DateTime)
     ($book . BookInput!))
    (startReading
     :arguments ((date . ($ date))
                 (book . ($ book)))
     title authors description isbn thumbnailUri
     rating review startedReading lastRead))))

(defconst finito--start-reading-mutation-variables
  "{
     \"date\": %s,
     \"book\": {
       \"title\": \"%s\",
       \"authors\": %s,
       \"description\": \"%s\",
       \"isbn\": \"%s\",
       \"thumbnailUri\": \"%s\"
     }
   }")

(defconst finito--finish-reading-mutation
  (graphql-mutation
   (:arguments
    (($date . DateTime)
     ($book . BookInput!))
    (finishReading
     :arguments ((date . ($ date))
                 (book . ($ book)))
     title authors description isbn thumbnailUri
     rating review startedReading lastRead))))

(defconst finito--finish-reading-mutation-variables
  "{
     \"date\": %s,
     \"book\": {
       \"title\": \"%s\",
       \"authors\": %s,
       \"description\": \"%s\",
       \"isbn\": \"%s\",
       \"thumbnailUri\": \"%s\"
     }
   }")

(defconst finito--delete-book-data-mutation
  (graphql-mutation
   (:arguments
    (($isbn . String!))
    (deleteBookData
     :arguments ((isbn . ($ isbn)))))))

(defconst finito--delete-book-data-mutation-variables "{\"isbn\": \"%s\"}")

(defconst finito--create-book-mutation
  (graphql-mutation
   (:arguments
    (($book . BookInput!))
    (createBook
     :arguments ((book . ($ book)))
     title))))

(defconst finito--create-book-mutation-variables
  "{
     \"book\": {
       \"title\": \"%s\",
       \"authors\": %s,
       \"description\": \"%s\",
       \"isbn\": \"%s\",
       \"thumbnailUri\": \"%s\"
     }
   }")

(defconst finito--series-query
  (graphql-query
   (:arguments
    (($book . BookInput!))
    (series
     :arguments ((book . ($ book)))
     title authors description isbn thumbnailUri
     rating review startedReading lastRead))))

(defconst finito--series-query-variables
  "{
     \"book\": {
       \"title\": \"%s\",
       \"authors\": %s,
       \"description\": \"%s\",
       \"isbn\": \"%s\",
       \"thumbnailUri\": \"%s\"
     }
   }")

(defconst finito--summary-query
  (graphql-query
   (:arguments
    (($from . DateTime)
     ($to . DateTime)
     ($montageInput . MontageInput)
     ($includeAdded . Boolean))
    (summary
     :arguments ((from . ($ from))
                 (to . ($ to))
                 (montageInput . ($ montageInput))
                 (includeAdded . ($ includeAdded)))
     read added averageRating montage))))

(defconst finito--summary-query-variables
  "{
     \"from\": %s,
     \"to\": %s,
     \"montageInput\": {
         \"columns\": %s,
         \"largeImageWidth\": %s,
         \"largeImageHeight\": %s,
         \"largeImgScaleFactor\": %s,
         \"largeImageRatingThreshold\": %s
     },
     \"includeAdded\": %s
   }")

(defun finito--books-pagination-input (limit offset)
  "Return a string BooksPaginationInput from LIMIT and OFFSET."
  (if (and limit offset)
      (format "{\"first\": %s, \"after\": %s}" limit offset)
    "null"))

(provide 'finito-graphql)
;;; finito-graphql.el ends here
