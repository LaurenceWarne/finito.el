;;; finito.el --- View books -*- lexical-binding: t -*-

;; Copyright (C) 2021 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Keywords: books
;; URL: https://github.com/LaurenceWarne/libro-finito
;; Package-Requires: ((emacs "27") (dash "2.17.0") (cl-lib "0.3") (request "0.3.2") (f "0.2.0") (s "1.12.0") (transient "0.3.5"))

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
;; An Emacs client for finito.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'org)
(require 'request)
(require 's)
(require 'transient)

(defgroup finito nil
  "Emacs client to fin"
  :group 'books)

(defcustom finito-insert-book-data
  #'finito--insert-book-data
  "A function which takes book data in the form of an alist, should process and insert it into the current buffer in some way, and then return the lines which contain the content."
  :group 'finito
  :type 'function)

(defcustom finito-image-cache-dir
  (f-join user-emacs-directory "finito-images/")
  "The directory used to cache images."
  :group 'finito
  :type 'string)

(defface finito-author-name
  '((t :foreground "aquamarine"
       :weight bold
       :underline t))
  "Face for author names."
  :group 'finito)

(defface finito-book-descriptions
  '((t :italic t))
  "Face for book descriptions."
  :group 'finito)

(defvar finito--host-uri "http://localhost:8080/api/graphql")

;;; Transients

(transient-define-prefix finito-search ()
  "Search for books."
  ["Variables"
   ("t" "Title" "title=" read-string)
   ("a" "Author" "author=" read-string)]
  ["Actions"
   ("c" "Copy Curl"     finito-request)
   ("s" "Search"        finito-request)])

(defun finito-request (&optional args)
  (interactive
   (list (transient-args 'finito-search)))
  ;; TODO how can I make this so that I don't have to parse the arg from arg-name=arg?
  (cl-flet* ((parse-arg (st) (car (last (s-split "=" st))))
             (get-arg (arg)
                      (parse-arg (or (--first (s-starts-with-p arg it) args) ""))))
    (let ((title-kws (get-arg "title"))
          (author-kws (get-arg "author")))
      (finito-search-for-books nil title-kws author-kws))))

(defun finito--get-request-plist (title-keywords author-keywords)
  "Return a plist with headers and body deduced from TITLE-KEYWORDS and AUTHOR-KEYWORDS."
  `(:headers
    (("Content-Type" . "application/json")
     ("Accept" . "application/json"))
    :data
    ,(format "{\"query\":\"\\nquery {\\n  books(titleKeywords: \\\"%s\\\", authorKeywords: \\\"%s\\\") {\\n    title\\n    author\\n    description\\n    isbn\\n    thumbnailUri\\n  }\\n}\"}" title-keywords author-keywords)))

(defun finito--make-request (request-plist)
  "Make a request for book data to `finito--host-uri' using REQUEST-PLIST and insert the contents into a new buffer after completion."
  (request finito--host-uri
    :headers (plist-get request-plist :headers)
    :data (plist-get request-plist :data)
    :parser 'json-read
    :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
    	           (message "Got error: %S" error-thrown)))
    :success (cl-function
    	      (lambda (&key data &allow-other-keys)
                (finito-process-data (cdadar data))))))

(defun finito-process-data (data)
  "Output the book data DATA in a buffer."
  (unless (f-dir-p finito-image-cache-dir) (f-mkdir finito-image-cache-dir))
  (switch-to-buffer "Books")
  (org-mode)
  (insert "* Books\n\n")
  ;; Vector to list)
  (-each (append data nil)
    (lambda (book)
      (let ((book-alist (finito--create-book-alist book)))
        (let-alist book-alist
          (if (f-exists-p .image-file-name)
              (funcall finito-insert-book-data book-alist)
            (message (concat "Retrieving img: " .img-uri))
            ;; this is already a callback so do we need to:
            ;; https://stackoverflow.com/questions/40504796/asynchrous-copy-file-and-copy-directory-in-emacs-lisp
            (url-copy-file .img-uri .image-file-name)
            (funcall finito-insert-book-data book-alist))))))
  (goto-char (point-min))
  ;; TODO should not be toggle, should be show
  (org-toggle-inline-images))

(defun finito--create-book-alist (book-response)
  "Return an alist containing book information gleaned from BOOK-RESPONSE.

The alist will contain the following keys:
title
author
description
isbn
img-uri
image-file-name"
  (let-alist book-response
    (let* ((title-de-spaced (s-replace " " "-" (downcase .title)))
           (image-file-name (f-join finito-image-cache-dir
                                    (concat title-de-spaced .isbn ".jpeg"))))
      `((title . ,.title)
        (author . ,.author)
        (description . ,.description)
        (isbn . ,.isbn)
        (img-uri . ,.thumbnailUri)
        (image-file-name . ,image-file-name)))))

(defun finito--insert-book-data (book-data-alist)
  "Insert into the current buffer contents from BOOK-DATA-ALIST."
  (let ((title (alist-get 'title book-data-alist))
        (author (alist-get 'author book-data-alist))
        (description (alist-get 'description book-data-alist))
        (image-file-name (alist-get 'image-file-name book-data-alist)))
    (insert (concat "** " title "\n\n"))
    (insert (concat "[[" image-file-name "]]  " author "\n\n"))
    (overlay-put (make-overlay (- (point) 2) (- (point) (length author) 2))
                 'face
                 'finito-author-name)
    (insert (concat description "\n\n"))
    (overlay-put (make-overlay (- (point) 2) (- (point) (length description) 2))
                 'face
                 'finito-book-descriptions)))

;;; Interactive functions

(defun finito-search-for-books (arg title-keywords author-keywords)
  "Search for books by title and author, and insert the results in a buffer.

Search for books matching TITLE-KEYWORDS and AUTHOR-KEYWORDS.  With any non-nil
prefix arg ARG, message an equivalent curl instead of sending a request."
  (interactive "P\nsPlease input title keywords: \nsPlease input author keywords: ")
  (if arg
      (let ((url (url-hexify-string (format "https://www.googleapis.com/books/v1/volumes?q=%s+inauthor:%s&printType=books&langRestrict=en" title-keywords author-keywords))))
        (kill-new (message url)))
    (let ((request-plist (finito--get-request-plist title-keywords author-keywords)))
      (finito--make-request request-plist))))

;; (finito-search-for-books nil "star" "david-brin")
;; (finito-process-data '(((title . "Flowers for Algernon") (author . "Daniel Keyes") (description . "'A masterpiece of poignant brilliance . . . heartbreaking' Guardian Charlie Gordon, a floor sweeper born with an unusually low IQ, has been chosen as the perfect subject for an experimental surgery that doctors hope will increase his intelligence - a procedure that has been highly successful when tested on a lab mouse named Algernon. All Charlie wants is to be smart and have friends, but the treatement turns him into a genius. Then Algernon begins to fade. What will become of Charlie?") (thumbnailUri . "http://books.google.com/books/content?id=VbOtAQAACAAJ&printsec=frontcover&img=1&zoom=1&source=gbs_api"))))

(provide 'finito)
;;; finito.el ends here
