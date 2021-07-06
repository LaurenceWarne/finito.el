;;; finito.el --- View and collect books in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Keywords: books
;; URL: https://github.com/LaurenceWarne/libro-finito
;; Package-Requires: ((emacs "27") (dash "2.17.0") (cl-lib "0.3") (request "0.3.2") (f "0.2.0") (s "1.12.0") (transient "0.3.5") (graphql "0.1.1") (llama "0.1.1"))

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
;; An Emacs interface for viewing and searching for books.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'llama)
(require 'org)
(require 'outline)
(require 'request)
(require 's)
(require 'transient)

(require 'finito-buffer)
(require 'finito-core)
(require 'finito-graphql)
(require 'finito-view)

(eval-when-compile (require 'let-alist))

(defcustom finito-writer-instance
  (finito-book-writer)
  "`finito-book-writer' instance to be used.

This instance will be used to write books in finito buffers."
  :group 'finito
  :type 'object)

(defcustom finito-image-cache-dir
  (f-join user-emacs-directory "finito-images/")
  "The directory used to cache images."
  :group 'finito
  :type 'string)

(defcustom finito-browse-function
  #'finito--browse-function
  "Function used by `finito-browse-book-at-point'.

It should take a book alist as a parameter."
  :group 'finito
  :type 'function)

(defconst finito--headers
  '(("Content-Type" . "application/json")
    ("Accept" . "application/json")))

(defvar finito--host-uri "http://localhost:8080/api/graphql")

;;; Buffer local variables

(defvar-local finito--buffer-books
  nil
  "An alist associating books to buffer lines they begin.")

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
                (concat "[" (mapconcat (##format "\"%s\"" %1) .authors ",") "]")
                (s-replace "\"" "'" .description)
                (s-replace "\"" "'" .isbn)
                (s-replace "\"" "'" .thumbnailUri))))))

(defun finito--make-request (request-plist callback)
  "Make a request to `finito--host-uri' using REQUEST-PLIST.

CALLBACK is called with the parsed json if the request is successful."
  (request finito--host-uri
    :headers (plist-get request-plist :headers)
    :data (plist-get request-plist :data)
    :parser 'json-read
    :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
    	           (message "Got error: '%s', is the server up?" error-thrown)))
    :success (cl-function
    	      (lambda (&key data &allow-other-keys)
                (let ((response-indicator (caadr data)))
                  (if (equal response-indicator 'errors)
                      ;; Error doesn't seem to do anything here
                      (message "Received error in gql response: %s" (cadr data))
                    (funcall callback (cdadar data))))))))

(defun finito--process-books-data (data)
  "Insert the books data DATA into a buffer."
  (finito--process
   data
   ;; Vector to list)
   (##-each (append % nil) #'finito--process-book-data)))

(defun finito--process-single-book (data)
  "Insert the book data DATA into a buffer."
  (finito--process data (##finito--process-book-data %)))

(defun finito--process (data callback)
  "Set up a finito book view buffer, and then call CALLBACK with DATA."
  (finito--buffer-create)
  (let ((inhibit-read-only t))
    (insert "* Books\n\n")
    (funcall callback data))
  (goto-char (point-min))
  (org-display-inline-images))

(defun finito--buffer-create ()
  "Create and switch to a finito book view buffer."
  (unless (f-dir-p finito-image-cache-dir) (f-mkdir finito-image-cache-dir))
  (switch-to-buffer (generate-new-buffer-name "Books"))
  (finito-book-view-mode))

(defun finito--process-book-data (book)
  "Insert data for BOOK into the current buffer."
  (add-to-list 'finito--buffer-books `(,(line-number-at-pos) . ,book))
  (let ((book-alist (finito--create-book-alist book)))
    (let-alist book-alist
      (unless (f-exists-p .image-file-name)
        (message (concat "Retrieving img: " .img-uri))
        ;; this is already a callback so do we need to:
        ;; https://stackoverflow.com/questions/40504796/asynchrous-copy-file-and-copy-directory-in-emacs-lisp
        (url-copy-file .img-uri .image-file-name))
      (finito-insert-book finito-book-writer-instance book-alist))))

(defun finito--create-book-alist (book-response)
  "Return an alist containing book information gleaned from BOOK-RESPONSE.

The alist will contain the following keys:
title
authors
description
isbn
img-uri
image-file-name"
  (let-alist book-response
    (let* ((title-sanitized
            (replace-regexp-in-string "[^A-Za-z0-9._-]" "" (s-downcase .title)))
           (image-file-name (f-join finito-image-cache-dir
                                    (concat title-sanitized .isbn ".jpeg"))))
      (list (cons 'title .title)
            (cons 'authors .authors)
            (cons 'description .description)
            (cons 'isbn .isbn)
            (cons 'img-uri .thumbnailUri)
            (cons 'image-file-name image-file-name)))))

(defun finito--insert-book-data (book-data-alist)
  "Insert into the current buffer contents from BOOK-DATA-ALIST."
  (let* ((title (alist-get 'title book-data-alist))
         (authors (alist-get 'authors book-data-alist))
         (authors-str (s-join ", " authors))
         (description (alist-get 'description book-data-alist))
         (image-file-name (alist-get 'image-file-name book-data-alist)))
    (insert (concat "** " title "\n\n"))
    (insert (concat "[[" image-file-name "]]  " authors-str "\n\n"))
    (overlay-put (make-overlay (- (point) 2) (- (point) (length authors-str) 2))
                 'face
                 'finito-author-name)
    (insert (concat description "\n\n"))
    (overlay-put (make-overlay (- (point) 2) (- (point) (length description) 2))
                 'face
                 'finito-book-descriptions)))

(defun finito--book-at-point ()
  "Get the book at the current point in the buffer."
  (when-let* ((line (line-number-at-pos))
              ;; Alist may not be ordered
              (books-before (--filter (<= (car it) line) finito--buffer-books)))
    (cdr (--max-by (> (car it) (car other)) books-before))))

(defun finito--select-collection (callback)
  "Prompt for a collection, and then call CALLBACK with that collection."
  (finito--make-request
     (finito--collections-request-plist)
     (lambda (response)
       (let* ((all-collections (-map #'cdar response))
              (chosen-collection (completing-read "Choose: " all-collections)))
         (funcall callback chosen-collection)))))

(defun finito--browse-function (book-alist)
  "Open a openlibrary page of a book, using it's isbn from BOOK-ALIST."
  (browse-url
   (concat "https://openlibrary.org/isbn/" (alist-get 'isbn book-alist))))

;;; Modes

(defvar finito-book-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "a" #'finito-add-book-at-point)  ; to default collection
    (define-key map "c" #'finito-add-book-at-point)
    (define-key map "A" #'finito-same-author)
    (define-key map "n" #'outline-next-heading)
    (define-key map "p" #'outline-previous-heading)
    (define-key map "o" #'finito-to-org-buffer)
    (define-key map "q" #'kill-current-buffer)
    (define-key map "k" #'kill-current-buffer)
    (define-key map "b" #'finito-browse-book-at-point)
    (define-key map "D" #'finito-delete-book-at-point)
    map))

(define-derived-mode finito-book-view-mode org-mode "finito-book-view"
  "A mode for showing Books.

The following commands are available in this mode:
\\{finito-book-view-mode-map}"
  (setq buffer-read-only     t
        finito--buffer-books nil)
  (buffer-disable-undo)
  (use-local-map finito-book-view-mode-map))

;;; Commands

(defun finito-same-author ()
  "Find books by the author at point."
  (interactive)
  (let* ((book (finito--book-at-point))
         (authors (s-join " " (alist-get 'authors book))))
    (message "Searching for author(s) '%s'" authors)
    (finito-search-for-books nil nil authors)))

(defun finito-to-org-buffer ()
  "Open the current buffer as a normal org mode buffer."
  (interactive)
  nil)

(defun finito-search-request (&optional args)
  "Send a search request to the finito server using transient args ARGS."
  (interactive
   (list (finito--transient-args-plist 'finito-search)))
  (if-let (isbn (plist-get args :isbn))
      (finito--make-request
       (finito--isbn-request-plist isbn)
       (##finito--process-single-book %))
    (finito-search-for-books
     nil
     (plist-get args :title)
     (plist-get args :author)
     (plist-get args :max-results))))

(defun finito-search-for-books
    (arg title-keywords author-keywords &optional max-results)
  "Search for books by title and author, and insert the results in a buffer.

Search for books matching TITLE-KEYWORDS and AUTHOR-KEYWORDS.  With any non-nil
prefix arg ARG, message an equivalent curl instead of sending a request."
  (interactive "P\nsPlease input title keywords: \nsPlease input author keywords: ")
  (if arg
      (let ((url (url-hexify-string (format "https://www.googleapis.com/books/v1/volumes?q=%s+inauthor:%s&printType=books&langRestrict=en" title-keywords author-keywords))))
        (kill-new (message url)))
    (let ((request-plist
           (finito--search-request-plist title-keywords author-keywords max-results)))
      (finito--make-request request-plist #'finito--process-books-data))))

(defun finito-create-collection (&optional _args)
  "Send a request to the finito server to create a new collection.

_ARGS does nothing and is needed to appease transient."
  (interactive)
  (if-let* ((name (read-string "Collection name: "))
            (request-plist (finito--create-collection-request-plist name)))
      (finito--make-request
       request-plist
       (##message "Successfully created collection '%s'" (cdar %1)))))

(defun finito-open-collection (&optional _args)
  "Prompt the user for a collection and open it.

_ARGS does nothing and is needed to appease transient."
  (interactive)
  (finito--select-collection
   (lambda (chosen-collection)
     (finito--make-request
      (finito--collection-request-plist chosen-collection)
      (##finito--process-books-data (cdar %))))))

(defun finito-delete-collection (&optional _args)
  "Prompt the user for a collection and delete it.

_ARGS does nothing and is needed to appease transient."
  (interactive)
  (finito--select-collection
     (lambda (chosen-collection)
       (finito--make-request
        (finito--delete-collection-request-plist chosen-collection)
        (lambda (_)
          (message "Successfully deleted collection '%s'" chosen-collection))))))

(defun finito-add-book-at-point ()
  "Prompt the user for a collection, and add the book at point to it."
  (interactive)
  (let ((book (finito--book-at-point)))
    (finito--select-collection
     (lambda (chosen-collection)
       (finito--make-request
        (finito--add-book-request-plist chosen-collection book)
        (lambda (_)
          (message "Successfully added '%s' to '%s'"
                   (alist-get 'title book)
                   chosen-collection)))))))

(defun finito-browse-book-at-point ()
  "Browse the book at point."
  (interactive)
  (funcall finito-browse-function (finito--book-at-point)))

(provide 'finito)
;;; finito.el ends here
