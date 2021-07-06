;;; finito-view.el --- GraphQL requests used by finito -*- lexical-binding: t -*-

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
;; This file contains utilities for working with libro finito's GraphQL api

;;; Code:

(require 'graphql)

(defconst finito--search-query
  (graphql-query
   (:arguments
    (($titleKeywords . String)
     ($authorKeywords . String)
     ($maxResults . Int))
    (books
     :arguments ((titleKeywords . ($ titleKeywords))
                 (authorKeywords . ($ authorKeywords))
                 (maxResults . ($ maxResults)))
     title authors description isbn thumbnailUri))))

;; We don't add quotes around %s since null is a value that could potentially
;; be placing, and we shouldn't quote that
(defconst finito--search-query-variables
  "{\"titleKeywords\": %s, \"authorKeywords\": %s, \"maxResults\": %s}")

(defconst finito--isbn-query
  (graphql-query
   (:arguments
    (($isbn . String!))
    (book
     :arguments ((isbn . ($ isbn)))
     title authors description isbn thumbnailUri))))

(defconst finito--isbn-query-variables "{\"isbn\": \"%s\"}")

(defconst finito--collection-query
  (graphql-query
   (:arguments
    (($name . String!))
    (collection
     :arguments ((name . ($ name)))
     (books title authors description isbn thumbnailUri)))))

(defconst finito--collection-query-variables "{\"name\": \"%s\"}")

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
     ($title . String!)
     ($authors . String!)
     ($description . String!)
     ($isbn . String!)
     ($thumbnailUri . String!))
    (addBook
     :arguments ((collection . ($ collection))
                 (book (title . ($ title))
                       (authors . ($ authors))
                       (description . ($ description))
                       (isbn . ($ isbn))
                       (thumbnailUri . ($ thumbnailUri))))
     name))))

(defconst finito--add-book-mutation-variables
  "{\"collection\": \"%s\", \"title\": \"%s\", \"authors\": %s, \"description\": \"%s\", \"isbn\": \"%s\", \"thumbnailUri\": \"%s\"}")

(defconst finito--delete-collection-mutation
  (graphql-mutation
   (:arguments
    (($name . String!))
    (deleteCollection
     :arguments ((name . ($ name)))))))

(defconst finito--delete-collection-mutation-variables "{\"name\": \"%s\"}")

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

(provide 'finito-graphql)
;;; finito-graphql.el ends here
