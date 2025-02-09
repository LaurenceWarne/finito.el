;;; finito.el --- View and collect books -*- lexical-binding: t -*-

;; Copyright (C) 2021 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.2.0
;; Keywords: outlines
;; URL: https://github.com/LaurenceWarne/finito.el
;; Package-Requires: ((emacs "27.1") (dash "2.19.1") (request "0.3.2") (f "0.2.0") (s "1.12.0") (transient "0.3.0") (graphql "0.1.1") (async "1.9.3"))

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
(require 'calendar)
(require 'cl-lib)
(require 'dash)
(require 'ewoc)
(require 'f)
(require 'json)
(require 'org)
(require 'outline)
(require 'request)
(require 's)
(require 'subr-x)
(require 'transient)

(require 'finito-buffer)
(require 'finito-core)
(require 'finito-request)
(require 'finito-server)

;;; Custom variables

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
URL `https://github.com/LaurenceWarne/libro-finito#special-collections' for
more information on how special collections can be configured."
  :group 'finito
  :type 'string)

(defcustom finito-currently-reading-collection
  "Currently Reading"
  "The name of the collection which holds books being currently read.

 This collection will be opened when the \"Currently Reading\" suffix is
invoked from the `finito' prefix command."
  :group 'finito
  :type 'string)

(defcustom finito-read-collection
  "Read"
  "The name of the collection which holds books which have been read.

 This collection will be opened when the \"Read\" suffix is
invoked from the `finito' prefix command."
  :group 'finito
  :type 'string)

(defcustom finito-add-book-collection-blacklist
  (list finito-currently-reading-collection)
  "Collections in this list will not show up on the add book collection prompt."
  :group 'finito
  :type '(repeat string))

(defcustom finito-montage-image-columns 6
  "Max number of images per row in a finito montage."
  :group 'finito
  :type 'integer)

(defcustom finito-montage-large-image-rating-threshold 5
  "Minimum rating value for images to be rendered as large on montages."
  :group 'finito
  :type 'integer)

(defcustom finito-init-summary-buffer-function #'finito--init-summary-buffer
  "Function used to generate a finito summary buffer.

It should take a book alist as a parameter which will contain the following
keys:

read
added
average-rating
montage-path"
  :group 'finito
  :type 'function)

(defcustom finito-summary-include-added
  t
  "If non-nil, include added books in addition to read books in montage images."
  :group 'finito
  :type 'boolean)

(defcustom finito-save-last-search
  t
  "If non-nil, save the arguments of the last `finito-search'."
  :group 'finito
  :type 'boolean)

;;; Constants

(defconst finito--montage-large-image-width 128)

(defconst finito--montage-large-image-height 196)

(defconst finito--montage-large-image-scale-factor 2)

;;; Internal variables

(defvar finito--special-collections
  (list finito-my-books-collection finito-currently-reading-collection))

(defvar finito--parallel-img-download t)

(defvar finito--sort-asc-alist '(("ascending" . true) ("descending" . false)))

;;; Internal functions

(cl-defun finito--make-request
    (request-plist callback &key sync graphql-error)
  "Make a request to `finito--host-uri' using REQUEST-PLIST.

CALLBACK is called with the parsed json if the request is successful.
If SYNC is non-nil make the request synchronous.  If errors are detected
in the graphql response body, then call GRAPHQL-ERROR with the first error
as a symbol."
  (when finito--debug (message "Finito request content: '%s'" request-plist))
  (request (finito--host-uri)
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
  "Insert the books data DATA into a buffer, and return the buffer.

Use INIT-OBJ, an instance of `finito-buffer-info' to initialize the buffer."
  (make-directory finito-img-cache-directory t)
  (let ((book-list (-map #'finito--create-book-alist (append data nil))))
    (cl-flet* ((callback (buffer-ewoc)
                         (-each book-list
                           (apply-partially #'ewoc-enter-last buffer-ewoc))
                         ;; ewoc adds a newline after each node and after
                         ;; the footer even if it is empty
                         (goto-char (point-max))
                         (while (s-blank-str-p (buffer-substring-no-properties
                                                (1- (point)) (point)))
                           (delete-char -1)))
               (proc-books () (finito--prepare-buffer init-obj #'callback)))
      (cond (finito-use-image-uris
             (proc-books))
            (finito--parallel-img-download
             (finito--download-images-par book-list #'proc-books))
            (t
             (finito--download-images book-list)
             (proc-books))))))

(defun finito--prepare-buffer (init-obj callback)
  "Prepare a finito buffer and return it.

Prepare a finito buffer using INIT-OBJ which should be a `finito-buffer-info'
instance, then call CALLBACK with an ewoc, which should use it to insert text
in some way, and then apply some final configuration to the buffer."
  (when (oref init-obj buf-name-unique)
    (ignore-errors (kill-buffer (oref init-obj buf-name))))
  (let ((buf (generate-new-buffer (oref init-obj buf-name))))
    (with-current-buffer buf
      (finito--benchmark finito--debug "Buffer insertion took: %ss"
        (finito-init-buffer init-obj)
        (let ((inhibit-read-only t)
              (buffer-ewoc
               (ewoc-create
                (lambda (obj) (finito-insert-book finito-writer-instance obj))
                (finito-title-string init-obj))))
          (setq finito--ewoc buffer-ewoc)
          (funcall callback buffer-ewoc))
        (let ((inhibit-message t))
          (toggle-truncate-lines -1))
        (goto-char (point-min))
        (org-display-inline-images)))
    (display-buffer buf '(display-buffer-same-window . nil))
    buf))

(defun finito--url-copy-file-log-error (url newname)
  "Copy URL to NEWNAME like `url-copy-file', but catch and log any errors."
  (condition-case nil
      (url-copy-file url newname)
    (error (message "Error downloading thumbnail '%s'" url))))

(defun finito--download-images (books)
  "Download the images for BOOKS."
  (--each
      (--remove (f-exists-p (alist-get 'image-file-name it)) books)
    (let-alist it (finito--url-copy-file-log-error .img-uri .image-file-name))))

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
                      (ignore-errors (url-copy-file ,.img-uri ,.image-file-name))
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
review
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
            (cons 'review .review)
            (cons 'started-reading .startedReading)
            (cons 'last-read .lastRead)))))

(defun finito--book-at-point ()
  "Get the book (as an alist) at the current point in the buffer.

The returned alist will match the format returned by
`finito--create-book-alist'."
  (unless (ewoc-nth finito--ewoc 0) (error "No books in the current buffer!"))
  (ewoc-data (ewoc-locate finito--ewoc)))

(defun finito--browse-function (book-alist)
  "Open an openlibrary page of a book, using the isbn in BOOK-ALIST.

BOOK-ALIST should be of the form returned by `finito--create-book-alist'."
  (browse-url
   (concat "https://openlibrary.org/isbn/" (alist-get 'isbn book-alist))))

(defun finito--open-specified-collection (collection &optional sync offset callback)
  "Open the collection COLLECTION in a view buffer and call CALLBACK.

If SYNC it non-nil, perform all actions synchronously.
OFFSET is the offset of the books in collection."
  (finito--make-request
   (finito--collection-request-plist
    collection (and (finito--use-pagination) finito-collection-books-limit) (or offset 0))
   (lambda (response)
     (with-current-buffer
         (finito--process-books-data
          (cdar response)
          (finito-collection-buffer-info
           :title collection
           :mode #'finito-collection-view-mode
           :buf-name (concat "Collection: " collection)
           :buf-name-unique t
           :books-offset (or offset 0)
           :total-books (or (ignore-errors (cdadar (last response))) 0)))
       (when callback (funcall callback))))
   :sync sync))

(defun finito--remove-book-region ()
  "Remove the book at point from the current buffer."
  (let ((inhibit-read-only t))
    (ewoc-delete finito--ewoc (ewoc-locate finito--ewoc))))

(defun finito--replace-book-at-point-from-request
    (plist &optional success-message)
  "Replace the book at point from a request.

Replace the book at point with the response of the request built using the
request plist PLIST.  When SUCCESS-MESSAGE is non-nil, message it if the
request is successful"
  (let ((node (ewoc-locate finito--ewoc))
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

(defun finito--get-summary-from-to ()
  "Return (from . to) for a summary request."
  (-let (((month day year) (calendar-current-date))
         ((_ _ from-year) (calendar-current-date -30)))
    (cons (format "%s-01-01" from-year) (format "%s-%02d-%02d" year month day))))

(defun finito--set-show-attribute-for-collection (alist-sym alist collection flag)
  "Set show the attribute implied by ALIST and ALIST-SYM to FLAG for COLLECTION."
  (if (member collection (mapcar #'car alist))
      (setf (cdr (assoc finito--collection alist)) flag)
    (set alist-sym `((,collection . ,flag) . ,alist))))

(defun finito--init-summary-buffer (summary-alist)
  "Create a summary buffer using data from SUMMARY-ALIST."
  (let ((buf (generate-new-buffer "finito summary")))
    (with-current-buffer buf
      (finito-summary-mode)
      (let ((inhibit-read-only t))
        (let-alist summary-alist
          (insert "* Year In Books\n\n")
          (insert (format "[[%s]]\n\n" .montage-path))
          (insert "** Your Statistics\n\n")
          (insert (format "- You've read %d" .read))
          (overlay-put
           (make-overlay (- (point) (length (number-to-string .read))) (point))
           'face 'finito-summary-read)
          (insert (format (format-time-string " books in %Y and added a total of %d.") .added))
          (overlay-put
           (make-overlay (1- (- (point) (length (number-to-string .added))))
                         (1- (point)))
           'face 'finito-summary-added)
          (let ((rating-str (if (= (floor .average-rating) .average-rating)
                                (number-to-string (floor .average-rating))
                              (format "%0.2f" .average-rating))))
            (insert (format "\n- You gave an average rating of %s." rating-str))
            (overlay-put
             (make-overlay (- (point) (length (number-to-string .read)) 1)
                           (1- (point)))
             'face 'finito-summary-average-rating))
          (when finito-summary-show-recommended
            (insert (concat "\n\n" finito--summary-recommended-text)))
          (org-display-inline-images)
          (goto-char (point-min)))))
    (display-buffer buf '(display-buffer-same-window . nil))))

(defun finito--ewoc-node-index (ewoc node)
  "Return the index of NODE in EWOC."
  (let* ((head-node (ewoc-nth ewoc 0))
         (cur-node head-node)
         (idx 0))
    (while (and (not (eq cur-node node)) (or (zerop idx) (not (eq cur-node head-node))))
      (setq cur-node (ewoc-next ewoc cur-node))
      (cl-incf idx))
    (and (eq node cur-node) idx)))

(defun finito--extract-collections-from-response (response &optional collection-filter)
  "Extract collections data from RESPONSE.

Output collections are further filtered by COLLECTION-FILTER."
  (-filter (or collection-filter (-const t))
           (-map #'cdar response)))

(defun finito--select-collection (callback &optional collection-filter sync)
  "Prompt for a collection, and then call CALLBACK with that collection.

If COLLECTION-FILTER is specified, only include collections in the prompt
for which COLLECTION-FILTER applied to the collection name evaluates to a
non-nil value.  If SYNC is non-nil, make the request synchronous."
  (finito--make-request
   (finito--collections-request-plist)
   (lambda (response)
     (let* ((all-collections (finito--extract-collections-from-response
                              response
                              collection-filter))
            ;; TODO check if any collections exist first
            (chosen-collection (completing-read "Choose: " all-collections)))
       (funcall callback chosen-collection)))
   :sync sync))

(defun finito--transient-select-collection (_prompt _initial-input _history)
  "Choose a collection."
  (let ((response (finito--make-request
                   (finito--collections-request-plist)
                   #'ignore
                   :sync t)))
    (completing-read "Choose: "
                     (finito--extract-collections-from-response
                      (alist-get 'collections (alist-get 'data (request-response-data response)))))))

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
    (define-key map "d" #'finito-toggle-show-descriptions)
    (define-key map "W" #'finito-toggle-show-reviews)
    (define-key map "M" #'finito-toggle-minimal)
    (define-key map "g" #'revert-buffer)
    (define-key map "l" #'finito-replay-search)
    (define-key map "R" #'finito-review-book-at-point)
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
    map))

(define-derived-mode finito-search-view-mode
  finito-view-mode
  "finito-search-view"
  "A mode for showing book search results.

The following commands are available in this mode:
\\{finito-search-view-mode-map}"
  (use-local-map finito-search-view-mode-map)
  (setq-local revert-buffer-function #'finito-search-revert)
  (setq-local finito--show-descriptions finito-show-descriptions-default))

(defvar finito-collection-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "D" #'finito-remove-book-at-point)
    (define-key map "N" #'finito-collection-next)
    (define-key map "P" #'finito-collection-previous)
    map))

(define-derived-mode finito-collection-view-mode
  finito-view-mode
  "finito-collection-view"
  "A mode for showing collections.

The following commands are available in this mode:
\\{finito-collection-view-mode-map}"
  (setq finito--collection nil)
  (use-local-map finito-collection-view-mode-map)
  (setq-local revert-buffer-function #'finito-collection-revert))

(defvar finito-summary-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "n" #'outline-next-heading)
    (define-key map "p" #'outline-previous-heading)
    (define-key map "o" #'finito-to-org-buffer)
    (define-key map "q" #'kill-current-buffer)
    (define-key map "k" #'kill-current-buffer)
    (define-key map (kbd "C-m") #'finito-open-my-books-collection)
    (define-key map (kbd "C-r") #'finito-open-currently-reading-collection)
    map))

(define-derived-mode finito-summary-mode org-mode "finito-summary"
  "A mode for viewing finito summaries.

The following commands are available in this mode:
\\{finito-summary-mode-map}"
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (use-local-map finito-summary-mode-map))

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
`https://developers.google.com/books/docs/v1/using') for the same
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
`https://developers.google.com/books/docs/v1/using') for a keyword
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
`https://developers.google.com/books/docs/v1/using') for the given ISBN.
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
(defun finito-open-read-collection (&optional _)
  "Open \"Read\"."
  (interactive)
  (finito--wait-for-server-then
   (finito--open-specified-collection finito-read-collection)))

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
     (when (xor sort-asc preferred-sort)
       (user-error "Both of sort ascending and preferred sort must be set if at least one is"))
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
    (display-buffer buf '(display-buffer-same-window . nil))
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
          (title (alist-get 'title book))
          (isbn (alist-get 'isbn book))
          (line (line-number-at-pos))
          (buf (current-buffer)))
     (when (y-or-n-p (format "Remove '%s' from %s?" title finito--collection))
       (finito--make-request
        (finito--remove-book-request-plist finito--collection isbn)
        (lambda (_)
          (message "Removed '%s' from %s"
                   (alist-get 'title book)
                   finito--collection)
          (finito--goto-buffer-line-and-remove-book-at-point buf line)))))))

(defun finito-collection-revert (&optional _ignore-auto _noconfirm)
  "Refresh the current collection, _IGNORE-AUTO and _NOCONFIRM are ignored."
  (interactive)
  (finito--wait-for-server-then
   (let ((collection finito--collection)
         (old-point (point))
         (node-idx (or (ignore-errors
                         (finito--ewoc-node-index finito--ewoc (ewoc-locate finito--ewoc)))
                       0))
         (request-backend 'url-retrieve))
     (kill-current-buffer)
     (finito--open-specified-collection
      collection
      t
      nil
      (lambda ()
        (unless (ignore-errors (ewoc-goto-node
                                finito--ewoc (ewoc-nth finito--ewoc node-idx)))
          (goto-char (min old-point (point-max))))
        (message "Refreshed collection '%s'" collection))))))

(defun finito-search-revert (&optional _ignore-auto _noconfirm)
  "Refresh the current search, _IGNORE-AUTO and _NOCONFIRM are ignored."
  (interactive)
  (let ((node (ewoc-locate finito--ewoc)))
    (ewoc-refresh finito--ewoc)
    (ewoc-goto-node finito--ewoc node))
  (org-display-inline-images))

(define-obsolete-function-alias
  'finito-refresh-collection
  'finito-collection-revert
  "0.5.0")

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

(defun finito-review-book-at-point ()
  "Add a review for the book at point.

Note this overwrites any existing review."
  (interactive)
  (finito--wait-for-server-then
   (let* ((book (finito--book-at-point))
          (coll-buf (current-buffer))
          (buf (generate-new-buffer (format "%s Review" (alist-get 'title book)))))
     (with-current-buffer buf
       ;; TODO make a major mode
       (local-set-key
        (kbd "C-c C-c")
        (lambda () (interactive)
          (finito--make-request
           (finito--review-book-request-plist
            book
            (json-encode-string (buffer-substring-no-properties (point-min) (point-max))))
           (lambda (&rest _)
             (with-current-buffer coll-buf (revert-buffer))
             (format "Added review for '%s'" (alist-get 'title book))))
          (kill-buffer-and-window)))
       (when-let ((review (alist-get 'review book)))
         (insert review)))
     (pop-to-buffer buf '(display-buffer-below-selected . nil))
     (message "Use C-c C-c to submit the review"))))

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
     (when (y-or-n-p
            (format
             "Remove held data for '%s' (note this won't remove the book from any collections)?"
             (alist-get 'title book)))
       (finito--make-request
        (finito--delete-book-data-request-plist (alist-get 'isbn book))
        (lambda (_)
          (message "Deleted info held about '%s'" (alist-get 'title book))
          (finito-collection-revert)))))))

(defun finito-replay-search ()
  "Open the search transient prefix with the last args that were used."
  (interactive)
  (let ((finito-save-last-search t))
    (call-interactively #'finito-search)))

(defun finito-collection-next ()
  "Get the next page of books for the current collection."
  (interactive)
  (when (finito--use-pagination)
    (let ((new-offset (+ finito--collection-books-current-offset
                         finito-collection-books-limit)))
      (if (>= new-offset finito--collection-total-books)
          (message "Already at the end of %s!" finito--collection)
        (finito--open-specified-collection finito--collection nil new-offset)))))

(defun finito-collection-previous ()
  "Get the previous page of books for the current collection."
  (interactive)
  (when (finito--use-pagination)
    (let ((new-offset (- finito--collection-books-current-offset
                         finito-collection-books-limit)))
      (if (< new-offset 0)
          (message "Already at the start of %s!" finito--collection)
        (finito--open-specified-collection finito--collection nil new-offset)))))

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

;;;###autoload
(defun finito-summary (&optional from to)
  "Open a summary buffer of reading highlights in the past year.

If either FROM or TO are specified, use these dates instead of the default of
the past year.

Example:
`(let ((finito-montage-image-columns 16))
   (finito-summary \"2021-01-01\" \"2099-01-01\"))'."
  (interactive)
  (-let (((default-from . default-to) (finito--get-summary-from-to)))
    (finito-summary-in-period (or from default-from) (or to default-to))))

;;;###autoload
(defun finito-summary-in-period (&optional from to)
  "Open a summary buffer of reading highlights between FROM and TO."
  (interactive)
  (finito--wait-for-server-then
   (let ((from (or from (org-read-date nil nil nil "From: ")))
         (to (or to (org-read-date nil nil nil "To:"))))
     (finito--make-request
      (finito--summary-request-plist
       from
       to
       finito-montage-image-columns
       finito--montage-large-image-width
       finito--montage-large-image-height
       finito--montage-large-image-scale-factor
       finito-montage-large-image-rating-threshold
       finito-summary-include-added)
      (lambda (response)
        (let-alist response
          (let ((montage-path (f-join finito-img-cache-directory "montage.png")))
            (f-write-bytes (base64-decode-string .montage) montage-path)
            (funcall finito-init-summary-buffer-function
                     (append `((montage-path . ,montage-path)
                               (average-rating . ,.averageRating))
                             response)))))))))

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

;; The setq-local make these next two hard to DRY
(defun finito-toggle-show-descriptions ()
  "Toggle display of descriptions."
  (interactive)
  (let ((local-val (bound-and-true-p finito--show-descriptions))
        (alist-val (when (bound-and-true-p finito--collection)
                     (alist-get finito--collection
                                finito-show-description-alist
                                finito-show-descriptions-default
                                nil
                                'equal))))
    (cond ((bound-and-true-p finito--collection)
           (finito--set-show-attribute-for-collection
            'finito-show-description-alist
            finito-show-description-alist
            finito--collection
            (not alist-val)))
          ((boundp 'finito--show-descriptions)
           (setq finito--show-descriptions (not local-val)))
          (t (setq-local finito--show-descriptions
                         (not finito-show-descriptions-default))))
    (let ((node (ewoc-locate finito--ewoc)))
      (ewoc-refresh finito--ewoc)
      (ewoc-goto-node finito--ewoc node))
    (org-display-inline-images)))

(defun finito-toggle-show-reviews ()
  "Toggle display of reviews."
  (interactive)
  (let ((local-val (bound-and-true-p finito--show-reviews))
        (alist-val (when (bound-and-true-p finito--collection)
                     (alist-get finito--collection
                                finito-show-review-alist
                                finito-show-reviews-default
                                nil
                                'equal))))
    (cond ((bound-and-true-p finito--collection)
           (finito--set-show-attribute-for-collection
            'finito-show-review-alist
            finito-show-review-alist
            finito--collection
            (not alist-val)))
          ((boundp 'finito--show-reviews)
           (setq finito--show-reviews (not local-val)))
          (t (setq-local finito--show-reviews
                         (not finito-show-reviews-default))))
    (let ((node (ewoc-locate finito--ewoc)))
      (ewoc-refresh finito--ewoc)
      (ewoc-goto-node finito--ewoc node))
    (org-display-inline-images)))

(defun finito-toggle-minimal ()
  "Toggle the book writer instance to/from a minimal book writer."
  (interactive)
  (setq finito-writer-instance
        (if (finito-minimal-book-writer-p finito-writer-instance)
            finito-detailed-writer-instance
          finito-minimal-writer-instance))
  (revert-buffer))

(defun finito-goodreads-import (csv-file)
  "Import the Goodreads export CSV-FILE.

In order to create an export, visit URL `https://www.goodreads.com/review/import'."
  (interactive "fSelect output CSV file: ")
  (finito--wait-for-server-then
   (message "Making request, this may take a while for large files!")
   (finito--make-request
    (finito--goodreads-import-request-plist
     (f-read-text csv-file))
    (lambda (response)
      (let-alist response
        (message "Finito import complete, Successful: %s, partially successful: %s, failed: %s"
                 (length .sucessful)
                 (length .partiallySuccessful)
                 (length .unsuccessful)))))))

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
  :argument "name="
  :reader #'finito--transient-select-collection)

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
   ("m" "📚 My Books"           finito-open-my-books-collection)
   ("r" "📖 Currently Reading"  finito-open-currently-reading-collection)
   ("R" "📕 Read"               finito-open-read-collection)
   ("s" "🔍 Search"             finito-search)
   ("o" "📁 Open a Collection"  finito-open-collection)
   ("c" "🚐 Collection Actions" finito-collection)])

(transient-define-prefix finito-search ()
  "Search for books."
  :class 'finito--search-prefix
  :incompatible '(("isbn=" "author=") ("isbn=" "title=") ("isbn=" "max results="))
  ["By Keywords"
   (finito--title-arg :description "Title" :prompt "Title: " :always-read t)
   (finito--author-arg :description "Author" :prompt "Author: " :always-read t)
   (finito--max-results-arg :description "Max Results" :prompt "Max results: " :always-read t)]
  ["Direct Lookup"
   (finito--isbn-arg :description "ISBN" :prompt "ISBN: " :always-read t)]
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

;; Has to be defined here to pass byte-compile for some reason
(defun finito--transient-args-plist (prefix)
  "Return the infixes of PREFIX as a plist."
  (-flatten-n 1 (--map (list (oref it plist-key)
                             (oref it value))
                       (-filter #'finito--transient-argument-p
                                (transient-suffixes prefix)))))

(provide 'finito)
;;; finito.el ends here
