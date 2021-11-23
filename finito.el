;;; finito.el --- View and collect books -*- lexical-binding: t -*-

;; Copyright (C) 2021 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.2.0
;; Keywords: outlines
;; URL: https://github.com/LaurenceWarne/finito.el
;; Package-Requires: ((emacs "27.1") (dash "2.17.0") (request "0.3.2") (f "0.2.0") (s "1.12.0") (transient "0.3.0") (graphql "0.1.1") (async "1.9.3"))

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

;; This package is an Emacs interface for viewing and searching for books.
;; Books are presented in modified org mode buffers, and books along with
;; user collections can be viewed/queried using transient.

;; The package's main entry point is `finito', a transient prefix command
;; from which collections can be created and viewed, and books searched
;; for.

;; The package is a thin client for
;; https://github.com/LaurenceWarne/libro-finito

;;; Code:

(require 'async)
(require 'cl-lib)
(require 'dash)
(require 'ewoc)
(require 'f)
(require 'org)
(require 'outline)
(require 'request)
(require 's)

(require 'finito-buffer)
(require 'finito-core)
(require 'finito-request)
(require 'finito-server)
(require 'finito-view)

;;; Custom variables

(defcustom finito-writer-instance
  (finito-book-writer)
  "This object will be used to write books in finito buffers."
  :group 'finito
  :type 'object)

(defcustom finito-img-cache-directory
  (f-join finito-server-directory "images/")
  "The directory of the finito image cache."
  :group 'finito
  :type 'directory)

(defcustom finito-browse-function
  #'finito--browse-function
  "Function used by `finito-browse-book-at-point'.

It should take a book alist (of the format returned by
`finito--create-book-alist') as a parameter."
  :group 'finito
  :type 'function)

(defcustom finito-keyword-search-buffer-init-instance
  (finito-buffer-info
   :title "Books"
   :mode (lambda () (finito-search-view-mode))
   :buf-name "Book Search"
   :buf-name-unique nil)
  "`finito-buffer-info' instance to be used.

This instance will be used to initialise a buffer after a keyword search."
  :group 'finito
  :type 'object)

(defcustom finito-my-books-collection
  "My Books"
  "The name of the \"default\" collection.

 This collection will be opened when the \"My Books\" suffix is invoked from
the `finito' prefix command.  Typically this collection will hold all
books which have been added at some point to some collection.  See
URL 'https://github.com/LaurenceWarne/libro-finito#special-collections' for
more information on how special collections can be configured."
  :group 'finito
  :type 'string)

(defcustom finito-currently-reading-collection
  "Currently Reading"
  "The name of the collection which holds books being currently read.

 This collection will be opened when the \"Currently Reading\" suffix is
invoked from the `finito' prefix command."
  :group 'finito
  :type 'object)

(defcustom finito-add-book-collection-blacklist
  (list finito-currently-reading-collection)
  "Collections in this list will not show up on the add book collection prompt."
  :group 'finito
  :type '(repeat string))

;;; Internal variables

(defvar finito--special-collections
  (list finito-my-books-collection finito-currently-reading-collection))

(defvar finito--parallel-img-download t)

;;; Misc functions

(cl-defun finito--make-request
    (request-plist callback &key sync graphql-error)
  "Make a request to `finito--host-uri' using REQUEST-PLIST.

CALLBACK is called with the parsed json if the request is successful.
If SYNC is non-nil make the request synchronous.  If errors are detected
in the graphql response body, then call GRAPHQL-ERROR with the first error
as a symbol."
  (request finito--host-uri
    :type "POST"
    :headers (plist-get request-plist :headers)
    :data (plist-get request-plist :data)
    :parser (lambda ()
              (set-buffer-multibyte t)
              (json-read))
    :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
    	           (message "Got error: '%s', is the server up?" error-thrown)))
    :success (cl-function
    	      (lambda (&key data &allow-other-keys)
                (let ((response-indicator (caadr data)))
                  (if (equal response-indicator 'errors)
                      (let ((first-error (elt (cdadr data) 0)))
                        (if graphql-error
                            (--> (cdr (-last-item (-last-item first-error)))
                              (s-dashed-words it)
                              (funcall graphql-error (intern it)))
                          ;; Error doesn't seem to do anything here
                          (message "Received error(s) in gql response: %s"
                                   (cdar first-error))))
                    (with-local-quit
                      (funcall callback (cdadar data)))))))
    :timeout (when sync 5)
    :sync sync))

(defun finito--process-books-data (data init-obj)
  "Insert the books data DATA into a buffer.

Use INIT-OBJ, an instance of `finito-buffer-info' to initialize the buffer."
  (make-directory finito-img-cache-directory t)
  (let ((book-list (-map #'finito--create-book-alist (append data nil))))
    (cl-flet* ((callback (buffer-ewoc)
                         (-each book-list
                           (apply-partially #'ewoc-enter-last buffer-ewoc))
                         ;; ewoc adds a newline after each node and after
                         ;; the footer even if it is empty
                         (goto-char (point-max))
                         (delete-char -3))
               (proc-books () (finito--prepare-buffer init-obj #'callback)))
      (cond (finito-use-image-uris
             (proc-books))
            (finito--parallel-img-download
             (finito--download-images-par book-list #'proc-books))
            (t
             (finito--download-images book-list)
             (proc-books))))))

(defun finito--prepare-buffer (init-obj callback)
  "Prepare a finito buffer.

Prepare a finito buffer using INIT-OBJ which should be a `finito-buffer-info'
instance, then call CALLBACK with an ewoc, which should use it to insert text
in some way, and then apply some final configuration to the buffer."
  (when (oref init-obj buf-name-unique)
    (ignore-errors (kill-buffer (oref init-obj buf-name))))
  (switch-to-buffer (generate-new-buffer-name (oref init-obj buf-name)))
  (finito-init-buffer init-obj)
  (let ((inhibit-read-only t)
        (buffer-ewoc
         (ewoc-create
          (lambda (obj) (finito-insert-book finito-writer-instance obj))
          (format "* %s\n" (oref init-obj title)))))
    (setq finito--ewoc buffer-ewoc)
    (funcall callback buffer-ewoc))
  (let ((inhibit-message t))
    (toggle-truncate-lines -1))
  (goto-char (point-min))
  (org-display-inline-images))

(defun finito--download-images (books)
  "Download the images for BOOKS."
  (--each
      (--remove (f-exists-p (alist-get 'image-file-name it)) books)
    (let-alist it (url-copy-file .img-uri .image-file-name))))

(defun finito--download-images-par (books callback)
  "Download the images for BOOKS in parallel and then call CALLBACK."
  (let ((filtered-books
         (--remove (f-exists-p (alist-get 'image-file-name it))
                   books))
        (handles))
    (if filtered-books
        (--map (let-alist it
                 (add-to-list
                  'handles
                  (async-start
                   `(lambda ()
                      (url-copy-file ,.img-uri ,.image-file-name)
                      ,.image-file-name)
                   (lambda (path)
                     (message "Downloaded '%s'" path)
                     (when (and (= (length filtered-books) (length handles))
                                (= 1 (length (-remove #'async-ready handles))))
                       (funcall callback))))))
               filtered-books)
      (funcall callback))))

(defun finito--create-book-alist (book-response)
  "Return an alist containing book information gleaned from BOOK-RESPONSE.

The alist will contain the following keys:
title
authors
description
isbn
img-uri
image-file-name
rating
started-reading
last-read"
  (let-alist book-response
    (let* ((title-sanitized
            (replace-regexp-in-string "[^A-Za-z0-9._-]" "" (s-downcase .title)))
           (image-file-name (f-join finito-img-cache-directory
                                    (concat title-sanitized .isbn ".jpeg"))))
      (list (cons 'title .title)
            (cons 'authors .authors)
            (cons 'description .description)
            (cons 'isbn .isbn)
            (cons 'img-uri .thumbnailUri)
            (cons 'image-file-name image-file-name)
            (cons 'rating .rating)
            (cons 'started-reading .startedReading)
            (cons 'last-read .lastRead)))))

(defun finito--book-at-point ()
  "Get the book (as an alist) at the current point in the buffer.

The returned alist will match the format returned by
`finito--create-book-alist'."
  (unless (ewoc-nth finito--ewoc 0) (error "No books in the current buffer!"))
  (ewoc-data (ewoc-locate finito--ewoc (point))))

(defun finito--select-collection (callback &optional collection-filter sync)
  "Prompt for a collection, and then call CALLBACK with that collection.

If COLLECTION-FILTER is specified, only include collections in the prompt
for which COLLECTION-FILTER applied to the collection name evaluates to a
non-nil value.  If SYNC is non-nil, make the request synchronous."
  (finito--make-request
   (finito--collections-request-plist)
   (lambda (response)
     (let* ((all-collections (-filter (or collection-filter (-const t))
                                      (-map #'cdar response)))
            ;; TODO check if any collections exist first
            (chosen-collection (completing-read "Choose: " all-collections)))
       (funcall callback chosen-collection)))
   :sync sync))

(defun finito--browse-function (book-alist)
  "Open an openlibrary page of a book, using the isbn in BOOK-ALIST.

BOOK-ALIST should be of the form returned by `finito--create-book-alist'."
  (browse-url
   (concat "https://openlibrary.org/isbn/" (alist-get 'isbn book-alist))))

(defun finito--open-specified-collection (collection &optional sync)
  "Open the collection COLLECTION in a view buffer.

If SYNC it non-nil, perform all actions synchronously."
  (finito--make-request
   (finito--collection-request-plist collection)
   (lambda (response)
     (finito--process-books-data
      (cdar response)
      (finito-collection-buffer-info
       :title collection
       :mode #'finito-collection-view-mode
       :buf-name (concat "Collection: " collection)
       :buf-name-unique t)))
   :sync sync))

(defun finito--remove-book-region ()
  "Remove the book at point from the current buffer."
  (let ((inhibit-read-only t))
    (ewoc-delete finito--ewoc (ewoc-locate finito--ewoc (point)))))

(defun finito--replace-book-at-point-from-request
    (plist &optional success-message)
  "Replace the book at point from a request.

Replace the book at point with the response of the request built using the
request plist PLIST.  When SUCCESS-MESSAGE is non-nil, message it if the
request is successful"
  (let ((node (ewoc-locate finito--ewoc (point)))
        (buf (current-buffer)))
    (finito--make-request
     plist
     (lambda (response)
       (when success-message (message success-message))
       (with-current-buffer buf
         (save-mark-and-excursion
           (let ((inhibit-read-only t)
                 (book-alist (finito--create-book-alist response)))
             (ewoc-set-data node book-alist)
             (ewoc-invalidate finito--ewoc node)
             (org-display-inline-images))))))))

(defun finito--goto-buffer-line-and-remove-book-at-point (buf line)
  "Go to the line LINE at buffer BUF, and remove the book at point."
  (with-current-buffer buf
    (save-mark-and-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (finito--remove-book-region))))

;;; Modes

(defvar finito-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "a" #'finito-add-book-at-point)
    (define-key map "m" #'finito-add-to-default-book-at-point)
    (define-key map "A" #'finito-same-author)
    (define-key map "n" #'outline-next-heading)
    (define-key map "p" #'outline-previous-heading)
    (define-key map "o" #'finito-to-org-buffer)
    (define-key map "q" #'kill-current-buffer)
    (define-key map "k" #'kill-current-buffer)
    (define-key map "b" #'finito-browse-book-at-point)
    (define-key map "r" #'finito-rate-book-at-point)
    (define-key map "s" #'finito-start-book-at-point)
    (define-key map "S" #'finito-start-and-date-book-at-point)
    (define-key map "f" #'finito-finish-book-at-point)
    (define-key map "F" #'finito-finish-and-date-book-at-point)
    (define-key map "e" #'finito-series-at-point)
    (define-key map "w" #'finito-title-of-book-at-point)
    (define-key map (kbd "C-m") #'finito-open-my-books-collection)
    (define-key map (kbd "C-r") #'finito-open-currently-reading-collection)
    map))

(define-derived-mode finito-view-mode org-mode "finito-view"
  "A mode for viewing books.

The following commands are available in this mode:
\\{finito-view-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map finito-view-mode-map))

(defvar finito-search-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "l" #'finito-replay-search)
    map))

(define-derived-mode finito-search-view-mode
  finito-view-mode
  "finito-search-view"
  "A mode for showing book search results.

The following commands are available in this mode:
\\{finito-search-view-mode-map}"
  (use-local-map finito-search-view-mode-map))

(defvar finito-collection-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "g" #'finito-refresh-collection)
    (define-key map "D" #'finito-remove-book-at-point)
    map))

(define-derived-mode finito-collection-view-mode
  finito-view-mode
  "finito-collection-view"
  "A mode for showing collections.

The following commands are available in this mode:
\\{finito-collection-view-mode-map}"
  (setq finito--collection nil)
  (use-local-map finito-collection-view-mode-map))

;;; Commands

(defun finito-search-request (&optional args)
  "Send a search request to the finito server using transient args ARGS."
  (interactive
   (list (finito--transient-args-plist 'finito-search)))
  (finito--wait-for-server-then
   (if-let ((isbn (plist-get args :isbn)))
       (finito--make-request
        (finito--isbn-request-plist isbn)
        (lambda (response)
          (finito--process-books-data
           response
           (finito-buffer-info
            :title (concat "ISBN: " isbn)
            :mode #'finito-search-view-mode
            :buf-name (concat "Search for ISBN: " isbn)
            :buf-name-unique t))))
     (finito-search-for-books
      (plist-get args :title)
      (plist-get args :author)
      (plist-get args :max-results)))))

(defun finito-search-request-curl-dbg (&optional args)
  "Copy to the kill ring a curl request for the search request args ARGS.

Copy to the kill ring a curl request string corresponding to what the server
would send to the Google Books API (see URL
'https://developers.google.com/books/docs/v1/using') for the same
set of args.  This is intended to be used for debugging."
  (interactive
   (list (finito--transient-args-plist 'finito-search)))
  (if (plist-get args :isbn)
      (finito-isbn-request-curl-dbg (plist-get args :isbn))
    (finito-kw-search-request-curl-dbg (plist-get args :title)
                                       (plist-get args :author))))

(defun finito-kw-search-request-curl-dbg (&optional title author)
  "Copy to the kill ring a curl request for a keyword search request.

Copy to the kill ring a curl request string corresponding to what the server
would send to the Google Books API (see URL
'https://developers.google.com/books/docs/v1/using') for a keyword
search using TITLE and AUTHOR keywords.  This is intended to be used for
debugging."
  (unless (or title author)
    (error "At least one of title and author must be specified"))
  (let* ((base-url "https://www.googleapis.com/books/v1/volumes?q=")
         (maybe-title (-some--> title
                         (url-hexify-string it)
                         (concat "intitle:" it)))
         (maybe-author (-some--> author
                         (url-hexify-string it)
                         (concat "inauthor:" it)))
         (kwords (-flatten (list maybe-title maybe-author)))
         (fields (list "title" "authors" "description"
                       "imageLinks" "industryIdentifiers"))
         (url-params (list (s-join "+" kwords)
                           "printType=books"
                           (concat "langRestrict=" finito-language)
                           (concat "fields=items/volumeInfo("
                                   (s-join "," fields)
                                   ")")))
         (url (concat base-url (s-join "&" url-params))))
    (kill-new (concat "curl -X GET " url))))

(defun finito-isbn-request-curl-dbg (isbn)
  "Copy to the kill ring a curl request for an isbn lookup request.

Copy to the kill ring a curl request string corresponding to what the server
would send to the Google Books API (see URL
'https://developers.google.com/books/docs/v1/using') for the given ISBN.
This is intended to be used for debugging."
  (let* ((base-url "https://www.googleapis.com/books/v1/volumes?q=")
         (url-params (list (concat "isbn:" isbn)
                           "printType=books"
                           (concat "langRestrict=" finito-language)))
         (url (concat base-url (s-join "&" url-params))))
    (kill-new (concat "curl -X GET " url))))

(defun finito-search-for-books
    (title-keywords author-keywords &optional max-results)
  "Search for books by title and author, and insert the results in a buffer.

Search for books matching TITLE-KEYWORDS and AUTHOR-KEYWORDS, and ask for a
maximum of MAX-RESULTS results."
  (interactive
   "P\nsPlease input title keywords: \nsPlease input author keywords: ")
  (finito--wait-for-server-then
   (let ((request-plist
          (finito--search-request-plist
           title-keywords
           author-keywords
           max-results)))
     (finito--make-request
      request-plist
      (lambda (data) (finito--process-books-data
                      data
                      finito-keyword-search-buffer-init-instance))))))

;;;###autoload
(defun finito-create-collection (&optional _)
  "Send a request to the finito server to create a new collection."
  (interactive)
  (finito--wait-for-server-then
   (if-let* ((name (read-string "Collection name: "))
             (request-plist (finito--create-collection-request-plist name)))
       (finito--make-request
        request-plist
        (lambda (c)
          (message "Created collection '%s'" (cdar c)))))))

;;;###autoload
(defun finito-open-collection (&optional _)
  "Prompt the user for a collection and open it."
  (interactive)
  (finito--wait-for-server-then
   (finito--select-collection #'finito--open-specified-collection)))

;;;###autoload
(defun finito-open-my-books-collection (&optional _)
  "Open \"My Books\"."
  (interactive)
  (finito--wait-for-server-then
   (finito--open-specified-collection finito-my-books-collection)))

;;;###autoload
(defun finito-open-currently-reading-collection (&optional _)
  "Open \"Currently Reading\"."
  (interactive)
  (finito--wait-for-server-then
   (finito--open-specified-collection finito-currently-reading-collection)))

;;;###autoload
(defun finito-delete-collection (&optional _)
  "Prompt the user for a collection and delete it."
  (interactive)
  (finito--wait-for-server-then
   (finito--select-collection
    (lambda (chosen-collection)
      (finito--make-request
       (finito--delete-collection-request-plist chosen-collection)
       (lambda (_)
         (message "Deleted collection '%s'" chosen-collection)))))))

;;;###autoload
(defun finito-update-collection-request (&optional args)
  "Update the collection specified in ARGS."
  (interactive
   (list (finito--transient-args-plist 'finito-update-collection)))
  (finito--wait-for-server-then
   (let ((chosen-collection (plist-get args :name))
         (new-name (plist-get args :new-name))
         (preferred-sort (-some--> (plist-get args :sort)
                           (s-replace " " "" it)))
         (sort-asc (alist-get (plist-get args :sort-ascending)
                              finito--sort-asc-alist
                              nil
                              nil
                              #'string=)))
     (finito--make-request
      (finito--update-collection-request-plist
       chosen-collection new-name preferred-sort sort-asc)
      (lambda (_)
        (message "Updated collection '%s'" chosen-collection))))))

(defun finito-same-author ()
  "Find books by the author of the book at point."
  (interactive)
  (let* ((book (finito--book-at-point))
         (book-authors (s-join " " (alist-get 'authors book))))
    (message "Searching for author(s) '%s'" book-authors)
    (finito-search-for-books nil book-authors)))

(defun finito-to-org-buffer ()
  "Open the current buffer as a normal org mode buffer."
  (interactive)
  (let ((buf (generate-new-buffer "finito org")))
    (copy-to-buffer buf (point-min) (point-max))
    (switch-to-buffer buf)
    (org-display-inline-images)))

(defun finito-add-book-at-point ()
  "Prompt the user for a collection, and add the book at point to it."
  (interactive)
  (finito--wait-for-server-then
   (let ((book (finito--book-at-point)))
     (finito--select-collection
      (lambda (chosen-collection)
        (finito--make-request
         (finito--add-book-request-plist book chosen-collection)
         (lambda (_)
           (message "Added '%s' to '%s'"
                    (alist-get 'title book)
                    chosen-collection))))
      (lambda (collection)
        (not (member collection finito-add-book-collection-blacklist)))))))

(defun finito-add-to-default-book-at-point ()
  "Add the book at point to the default collection."
  (interactive)
  (finito--wait-for-server-then
   (let ((book (finito--book-at-point)))
     (finito--make-request
      (finito--add-book-request-plist book)
      (lambda (_)
        (message "Added '%s'" (alist-get 'title book)))))))

(defun finito-remove-book-at-point ()
  "Remove the book at point from the current collection."
  (interactive)
  (finito--wait-for-server-then
   (let* ((book (finito--book-at-point))
          (isbn (alist-get 'isbn book))
          (line (line-number-at-pos))
          (buf (current-buffer)))
     (finito--make-request
      (finito--remove-book-request-plist finito--collection isbn)
      (lambda (_)
        (message "Removed '%s' from %s"
                 (alist-get 'title book)
                 finito--collection)
        (finito--goto-buffer-line-and-remove-book-at-point buf line))))))

(defun finito-refresh-collection ()
  "Refresh the current collection."
  (interactive)
  (finito--wait-for-server-then
   (let ((collection finito--collection)
         (old-point (point))
         (request-backend 'url-retrieve))
     (kill-current-buffer)
     (finito--open-specified-collection collection t)
     (goto-char old-point)
     (message "Refreshed collection '%s'" collection))))

(defun finito-browse-book-at-point ()
  "Browse the book at point."
  (interactive)
  (funcall finito-browse-function (finito--book-at-point)))

(defun finito-rate-book-at-point ()
  "Rate the book at point."
  (interactive)
  (finito--wait-for-server-then
   (let ((book (finito--book-at-point))
         (rating (read-string "Rating: ")))
     (finito--replace-book-at-point-from-request
      (finito--rate-book-request-plist book rating)
      (format "Gave '%s' a rating of %s"
              (alist-get 'title book)
              rating)))))

(defun finito-start-book-at-point (&optional date)
  "Mark the book at point as currently reading.

When DATE is specified, mark that as the date the book was started."
  (interactive)
  (finito--wait-for-server-then
   (let ((book (finito--book-at-point)))
     (finito--replace-book-at-point-from-request
      (finito--start-reading-request-plist book date)
      (format "Added '%s' to currently reading"
              (alist-get 'title book))))))

(defun finito-start-and-date-book-at-point ()
  "Mark the book at point as currently reading from a prompted date."
  (interactive)
  (finito-start-book-at-point (org-read-date)))

(defun finito-finish-book-at-point (&optional date)
  "Mark the book at point as finished.

When DATE is specified, mark that as the date the book was finished."
  (interactive)
  (finito--wait-for-server-then
   (let* ((book (finito--book-at-point))
          (request-plist (finito--finish-reading-request-plist book date))
          (msg (format "Marked '%s' as finished" (alist-get 'title book))))
     (if (string= finito--collection finito-currently-reading-collection)
         (let ((line (line-number-at-pos))
               (buf (current-buffer)))
           (finito--make-request
            request-plist
            (lambda (_)
              (finito--goto-buffer-line-and-remove-book-at-point buf line)
              (message msg))))
       (finito--replace-book-at-point-from-request request-plist msg)))))

(defun finito-finish-and-date-book-at-point ()
  "Mark the book at point as finished on a prompted date."
  (interactive)
  (finito-finish-book-at-point (org-read-date)))
  
(defun finito-delete-data-for-book-at-point ()
  "Remove all data held about the book at point."
  (interactive)
  (finito--wait-for-server-then
   (let ((book (finito--book-at-point)))
     (finito--make-request
      (finito--delete-book-data-request-plist (alist-get 'isbn book))
      (lambda (_)
        (message "Deleted info held about '%s'" (alist-get 'title book))
        (finito-refresh-collection))))))

(defun finito-replay-search ()
  "Open the search transient prefix with the last args that were used."
  (interactive)
  (let ((finito-save-last-search t))
    (call-interactively #'finito-search)))

;;;###autoload
(defun finito-create-book (title authors description img-uri isbn)
  "Create a new book.

Use TITLE, AUTHORS, DESCRIPTION, IMG-URI and ISBN to create a new book.
The book will be added to `finito-my-books-collection'.

IMG-URI should point to a 128*195 image to be consistent with the sizes of
the other images.  You can do this for example using ImageMagick via:

convert original.png -resize 128x195! new.png."
  (interactive "sPlease input the book title:
sPlease input the book authors:
sPlease input the book description:
sPlease input an image url to be used:
sPlease input a unique identifier (used in place of an isbn):")
  (finito--wait-for-server-then
   (let ((book `((title . ,title)
                 (authors . ,(s-split "," (s-replace ", " "," authors)))
                 (description . ,description)
                 (img-uri . ,img-uri)
                 (isbn . ,isbn))))
     (finito--make-request
      (finito--create-book-request-plist book)
      (lambda (_)
        (message "Created the custom book '%s'" title)
        (finito--make-request
         (finito--add-book-request-plist book finito-my-books-collection)
         #'ignore))))))

(defun finito-series-at-point ()
  "Find books in the same series as the book at point."
  (interactive)
  (finito--wait-for-server-then
   (let* ((book (finito--book-at-point))
          (title (alist-get 'title book)))
     (message "Searching for books in the same series as '%s'" title)
     (finito--make-request
      (finito--series-request-plist book)
      (lambda (data)
        (if (= 0 (length data))
            (message "Could not find a book series containing '%s'" title)
          (finito--process-books-data
           data
           finito-keyword-search-buffer-init-instance)))))))

(defun finito-open-playground ()
  "Open the finito server's graphql playground - useful for debugging."
  (interactive)
  (finito--wait-for-server-then
   (browse-url (concat finito--base-uri "/graphiql"))))

(defun finito-title-of-book-at-point ()
  "Copy the title of the book at point to the kill ring."
  (interactive)
  (finito--wait-for-server-then
   (let* ((book (finito--book-at-point))
          (title (alist-get 'title book)))
     (kill-new title)
     (message "Copied '%s' to the kill ring" title))))

(provide 'finito)
;;; finito.el ends here
