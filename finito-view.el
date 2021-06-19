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

(require 's)
(require 'transient)

;;; Transients

(transient-define-prefix finito-dispatch ()
  "Search for books."
  ["Actions"
   ("s" "Search"      finito-search)
   ("c" "Collections" finito-collection)])

(transient-define-prefix finito-search ()
  "Search for books."
  ["Variables"
   ("t" "Title"  "title="  read-string)
   ("a" "Author" "author=" read-string)
   ("i" "isbn"   "isbn="   read-string)]
  ["Actions"
   ;("c" "Copy Curl"     finito-request)
   ("s" "Search"        finito-request)])

(transient-define-prefix finito-collection ()
  "Search for books."
  ["Actions"
   ;("c" "Copy Curl"     finito-request)
   ("n" "Create a new Collection" ignore)
   ("o" "Open a Collection"       ignore)
   ("d" "Delete a Collection"     ignore)
   ("i" "Import a Collection"     ignore)
   ("e" "Export a Collection"     ignore)])

(provide 'finito-view)
;;; finito-view.el ends here
