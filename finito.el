;;; finito.el --- View and collect books in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Keywords: books
;; URL: https://github.com/LaurenceWarne/libro-finito
;; Package-Requires: ((emacs "27.1") (dash "2.17.0") (request "0.3.2") (f "0.2.0") (s "1.12.0") (transient "0.3.5") (graphql "0.1.1") (llama "0.1.1") (async "1.9.3"))

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

(require 'finito-buffer)
(require 'finito-core)
(require 'finito-request)
(require 'finito-server)
(require 'finito-view)

(eval-when-compile (require 'let-alist))

;;; Custom variables

(defcustom finito-writer-instance
  (finito-book-writer)
  "`finito-book-writer' instance to be used.

This object will be used to write books in finito buffers."
  :group 'finito
  :type 'object)

(defcustom finito-image-cache-dir
  (f-join user-emacs-directory "finito/images/")
  "The directory used to cache images."
  :group 'finito
  :type 'string)

(defcustom finito-browse-function
  #'finito--browse-function
  "Function used by `finito-browse-book-at-point'.

It should take a book alist as a parameter."
  :group 'finito
  :type 'function)

(defcustom finito-keyword-search-buffer-init-instance
  (finito-buffer-info :title "Books" :mode (lambda () (finito-search-view-mode)))
  "`finito-buffer-info' instance to be used.

This instance will be used to initialise a buffer after a keyword search."
  :group 'finito
  :type 'object)

(defcustom finito-my-books-collection
  "My Books"
  "The name of the \"default\" collection.

 This collection will be opened when the \"My Books\" suffix is invoked from
the `finito-dispatch' prefix command.  Typically this collection will hold all
books which have been added at some point to some collection.  See
URL 'https://github.com/LaurenceWarne/libro-finito#special-collections' for
more information on how special collections can be configured."
  :group 'finito
  :type 'string)

(defcustom finito-currently-reading-collection
  "Currently Reading"
  "The name of the collection which holds books being currently read.

 This collection will be opened when the \"Currently Reading\" suffix is
invoked from the `finito-dispatch' prefix command."
  :group 'finito
  :type 'object)

;;; Internal variables

(defvar finito--special-collections
  (list finito-my-books-collection finito-currently-reading-collection))

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
    :parser 'json-read
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
                    (funcall callback (cdadar data))))))
    :timeout (when sync 5)
    :sync sync))

(defun finito--process-books-data (data init-obj)
  "Insert the books data DATA into a buffer.

Use INIT-OBJ, an instance of `finito-buffer-init' to initialize the buffer."
  (finito--process
   init-obj
   ;; (append data nil) converts vector to a list
   (lambda () (-each (append data nil)
                (-compose #'finito--layout-book-data
                          #'finito--create-book-alist)))))

(defun finito--process-single-book (data init-obj)
  "Insert the book data DATA into a buffer.

Use INIT-OBJ, an instance of `finito-buffer-init' to initialize the buffer."
  (finito--process init-obj (lambda () (finito--layout-book-data
                                        (finito--create-book-alist data)))))

(defun finito--process (init-obj callback)
  "Set up a finito buffer.

Set up a finito buffer using INIT-OBJ which should be a `finito-buffer-init'
instance, then call CALLBACK which should insert text in some way, and
then apply some final configuration to the buffer."
  (make-directory finito-image-cache-dir t)
  (switch-to-buffer (generate-new-buffer-name "Books"))
  (finito-init-buffer init-obj)
  (let ((inhibit-read-only t))
    (insert (format "* %s\n\n" (oref init-obj title)))
    (funcall callback))
  (goto-char (point-min))
  (org-display-inline-images))

(defun finito--layout-book-data (book-alist)
  "Insert data for BOOK-ALIST into the current buffer.

BOOK-ALIST should be an alist of the format produced by
`finito--create-book-alist'."
  (add-to-list 'finito--buffer-books `(,(line-number-at-pos) . ,book-alist))
  (let-alist book-alist
    (let ((display-remote (bound-and-true-p org-display-remote-inline-images)))
      (unless (or (and display-remote (not (eq display-remote 'skip)))
                  (f-exists-p .image-file-name))
        (message (concat "Retrieving img: " .img-uri))
        ;; TODO this is already a callback so do we need to:
        ;; https://stackoverflow.com/questions/40504796/asynchrous-copy-file-and-copy-directory-in-emacs-lisp
        (url-copy-file .img-uri .image-file-name)))
    (finito-insert-book finito-writer-instance book-alist)))

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
           (image-file-name (f-join finito-image-cache-dir
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
  (unless finito--buffer-books (error "No books in the current buffer!"))
  (let* ((line (line-number-at-pos))
         ;; Alist may not be ordered
         (books-before (--filter (<= (car it) line) finito--buffer-books)))
    (unless books-before (error "Cursor is not at a book!"))
    (cdr (--max-by (> (car it) (car other)) books-before))))

(defun finito--books-filter (pred books)
  "Remove books matching PRED from BOOKS.

BOOKS is expected to be in the format of `finito--buffer-books.'"
  (let* ((sorted-books (--sort (< (car it) (car other)) books))
         (diffs (finito--diffs (-map #'car sorted-books))))
    (cdr (-reduce-from
          (lambda (acc book-cons)
            (-let (((index . book) book-cons)
                   ((sub-acc . books-acc) acc))
              (if (funcall pred book)
                  (cons sub-acc (-snoc books-acc (cons (- index sub-acc) book)))
                (let* ((book-idx (-elem-index book-cons sorted-books))
                       ;; goes to default of 0 iff it's the last element
                       (diff (or (nth (1+ book-idx) diffs) 0)))
                  (cons (+ diff sub-acc) books-acc)))))
          '(0 . nil)
          sorted-books))))

(defun finito--diffs (list)
  "Return a list of diffs of LIST."
  (car (--reduce-from
        (-let (((acc-list . prev) acc))
          (cons (-snoc acc-list (- it prev)) it))
        '(nil . 0)
        list)))

(defun finito--select-collection (callback &optional collection-filter)
  "Prompt for a collection, and then call CALLBACK with that collection.

If COLLECTION-FILTER is specified, only include collections in the prompt
for which COLLECTION-FILTER applied to the collection name evaluates to a
non-nil value."
  (finito--make-request
     (finito--collections-request-plist)
     (lambda (response)
       (let* ((all-collections (-filter (or collection-filter (-const t))
                                        (-map #'cdar response)))
              ;; TODO check if any collections exist first
              (chosen-collection (completing-read "Choose: " all-collections)))
         (funcall callback chosen-collection)))))

(defun finito--browse-function (book-alist)
  "Open an openlibrary page of a book, using the isbn in BOOK-ALIST."
  (browse-url
   (concat "https://openlibrary.org/isbn/" (alist-get 'isbn book-alist))))

(defun finito--open-specified-collection (collection &optional sync)
  "Open the collection COLLECTION in a view buffer.

If SYNC it non-nil, perform all actions synchronously."
  (finito--make-request
   (finito--collection-request-plist collection)
   (##finito--process-books-data
    (cdar %)
    (finito-collection-buffer-info :title collection
                                   :mode #'finito-collection-view-mode))
   :sync sync))

(defun finito--remove-book-region ()
  "Remove the book at point from the current buffer."
  (let* ((book (finito--book-at-point))
         (books finito--buffer-books)
         (indices (-sort #'< (-map #'car books)))
         (idx (--find-last-index (<= it (line-number-at-pos)) indices))
         (book-start-line (nth idx indices))
         (inhibit-read-only t))
    ;; There's got to be a better way...
    (goto-char (point-min))
    (forward-line (1- book-start-line))
    (--dotimes (- (or (nth (1+ idx) indices)
                      (line-number-at-pos (point-max)))
                  book-start-line)
      (delete-region (point) (1+ (line-end-position))))
    (setq finito--buffer-books
          (finito--books-filter (##not (equal % book)) books))))

(defun finito--insert-book-in-current-buffer (book)
  "Insert BOOK at point the current buffer.

BOOK should be a unparsed server response book alist.  This function will
also update `finito--buffer-books' as necessary."
  (let ((line-num (line-number-at-pos))
        (book-alist (finito--create-book-alist book))
        (inhibit-read-only t))
    (finito--layout-book-data book-alist)
    (let ((diff (- (line-number-at-pos) line-num)))
      (setq finito--buffer-books
            (--map-when (and (>= (car it) line-num)
                             (not (equal (cdr it) book-alist)))
                        (cons (+ (car it) diff) (cdr it))
                        finito--buffer-books)))
    (org-display-inline-images)))

(defun finito--replace-book-at-point-from-request
    (plist &optional success-message)
  "Replace the book at point from a request.

Replace the book at point with the response of the request built using the
request plist PLIST.  When SUCCESS-MESSAGE is non-nil, message it if the
request is successful"
  (let ((line (line-number-at-pos))
        (buf (current-buffer)))
    (finito--make-request
     plist
     (lambda (response)
       (when success-message (message success-message))
       (with-current-buffer buf
         (save-mark-and-excursion
           (goto-char (point-min))
           (forward-line (1- line))
           (finito--remove-book-region)
           (finito--insert-book-in-current-buffer response)))))))

(defun finito--goto-buffer-line-and-remove-book-at-point (buf line)
  "Go to the line LINE at buffer BUF, and remove the book at point."
  (with-current-buffer buf
    (save-mark-and-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (finito--remove-book-region))))

;;; Modes

(defvar finito-search-view-mode-map
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
    map))

(define-derived-mode finito-search-view-mode org-mode "finito-search-view"
  "A mode for showing book search results.

The following commands are available in this mode:
\\{finito-search-view-mode-map}"
  (setq buffer-read-only     t
        finito--buffer-books nil)
  (buffer-disable-undo)
  (use-local-map finito-search-view-mode-map))

(defvar finito-collection-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "g" #'finito-refresh-collection)
    (define-key map "D" #'finito-remove-book-at-point)
    map))

(define-derived-mode finito-collection-view-mode
  finito-search-view-mode
  "finito-collection-view"
  "A mode for showing collections.

The following commands are available in this mode:
\\{finito-collection-view-mode-map}"
  (setq finito--collection nil)
  (buffer-disable-undo)
  (use-local-map finito-collection-view-mode-map))

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
  (let ((buf (generate-new-buffer "finito org")))
    (copy-to-buffer buf (point-min) (point-max))
    (switch-to-buffer buf)
    (org-display-inline-images)))

(defun finito-search-request (&optional args)
  "Send a search request to the finito server using transient args ARGS."
  (interactive
   (list (finito--transient-args-plist 'finito-search)))
  (if-let (isbn (plist-get args :isbn))
      (finito--make-request
       (finito--isbn-request-plist isbn)
       (##finito--process-single-book
        %
        (finito-buffer-info :title (concat "ISBN: " isbn)
                            :mode #'finito-search-view-mode)))
    (finito-search-for-books
     nil
     (plist-get args :title)
     (plist-get args :author)
     (plist-get args :max-results))))

;;;###autoload
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
      (finito--make-request
       request-plist
       (lambda (data) (finito--process-books-data
                       data
                       finito-keyword-search-buffer-init-instance))))))

;;;###autoload
(defun finito-create-collection (&optional _args)
  "Send a request to the finito server to create a new collection.

_ARGS does nothing and is needed to appease transient."
  (interactive)
  (if-let* ((name (read-string "Collection name: "))
            (request-plist (finito--create-collection-request-plist name)))
      (finito--make-request
       request-plist
       (##message "Successfully created collection '%s'" (cdar %1)))))

;;;###autoload
(defun finito-open-collection (&optional _args)
  "Prompt the user for a collection and open it.

_ARGS does nothing and is needed to appease transient."
  (interactive)
  (finito--select-collection (##finito--open-specified-collection %1)))

;;;###autoload
(defun finito-my-books (&optional _args)
  "Open \"My Books\".

_ARGS does nothing and is needed to appease transient."
  (interactive)
  (finito--open-specified-collection finito-my-books-collection))

;;;###autoload
(defun finito-open-currently-reading-collection (&optional _args)
  "Open \"Currently Reading\".

_ARGS does nothing and is needed to appease transient."
  (interactive)
  (finito--open-specified-collection finito-currently-reading-collection))

;;;###autoload
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

;;;###autoload
(defun finito-update-collection-request (&optional args)
  "Update the collection specified in ARGS."
  (interactive
   (list (finito--transient-args-plist 'finito-update-collection)))
  (let ((chosen-collection (plist-get args :name))
        (new-name (plist-get args :new-name))
        (preferred-sort (plist-get args :sort)))
    (finito--make-request
     (finito--update-collection-request-plist
      chosen-collection new-name preferred-sort)
     (lambda (_)
       (message "Successfully updated collection '%s'" chosen-collection)))))

(defun finito-add-book-at-point ()
  "Prompt the user for a collection, and add the book at point to it."
  (interactive)
  (let ((book (finito--book-at-point)))
    (finito--select-collection
     (lambda (chosen-collection)
       (finito--make-request
        (finito--add-book-request-plist book chosen-collection)
        (lambda (_)
          (message "Successfully added '%s' to '%s'"
                   (alist-get 'title book)
                   chosen-collection)))))))

(defun finito-add-to-default-book-at-point ()
  "Add the book at point to the default collection."
  (interactive)
  (let ((book (finito--book-at-point)))
    (finito--make-request
     (finito--add-book-request-plist book)
     (lambda (_) (message "Successfully added '%s'" (alist-get 'title book))))))

(defun finito-remove-book-at-point ()
  "Remove the book at point from the current collection."
  (interactive)
  (let* ((book (finito--book-at-point))
         (isbn (alist-get 'isbn book))
         (line (line-number-at-pos))
         (buf (current-buffer)))
    (finito--make-request
     (finito--remove-book-request-plist finito--collection isbn)
     (lambda (_)
       (message "Removed '%s' from %s" (alist-get 'title book) finito--collection)
       (finito--goto-buffer-line-and-remove-book-at-point buf line)))))

(defun finito-refresh-collection ()
  "Refresh the current collection."
  (interactive)
  (let ((collection finito--collection)
        (old-point (point))
        (request-backend 'url-retrieve))
    (kill-current-buffer)
    (finito--open-specified-collection collection t)
    (goto-char old-point)))

(defun finito-browse-book-at-point ()
  "Browse the book at point."
  (interactive)
  (funcall finito-browse-function (finito--book-at-point)))

(defun finito-rate-book-at-point ()
  "Rate the book at point."
  (interactive)
  (let ((book (finito--book-at-point))
        (rating (read-string "Rating: ")))
    (finito--replace-book-at-point-from-request
     (finito--rate-book-request-plist book rating)
     (format "Successfully gave '%s' a rating of %s"
             (alist-get 'title book)
             rating))))

(defun finito-start-book-at-point (&optional date)
  "Mark the book at point as currently reading.

When DATE is specified, mark that as the date the book was started."
  (interactive)
  (let ((book (finito--book-at-point)))
    (finito--replace-book-at-point-from-request
     (finito--start-reading-request-plist book date)
     (format "Successfully added '%s' to currently reading"
             (alist-get 'title book)))))

(defun finito-start-and-date-book-at-point ()
  "Mark the book at point as currently reading from a prompted date."
  (interactive)
  (finito-start-book-at-point (org-read-date)))

(defun finito-finish-book-at-point (&optional date)
  "Mark the book at point as finished.

When DATE is specified, mark that as the date the book was finished."
  (interactive)
  (let* ((book (finito--book-at-point))
         (request-plist (finito--finish-reading-request-plist book date))
         (msg (format "Successfully marked '%s' as finished"
                      (alist-get 'title book))))
    (if (string= finito--collection finito-currently-reading-collection)
        (let ((line (line-number-at-pos))
              (buf (current-buffer)))
          (finito--make-request
           request-plist
           (lambda (_)
             (finito--goto-buffer-line-and-remove-book-at-point buf line)
             (message msg))))
      (finito--replace-book-at-point-from-request request-plist msg))))

(defun finito-finish-and-date-book-at-point ()
  "Mark the book at point as finished on a prompted date."
  (interactive)
  (finito-finish-book-at-point (org-read-date)))
  
(defun finito-delete-data-for-book-at-point ()
  "Remove all data held about the book at point."
  (interactive)
  (let ((book (finito--book-at-point)))
    (finito--make-request
     (finito--delete-book-data-request-plist (alist-get 'isbn book))
     (lambda (_)
       (finito--replace-book-at-point-from-request
        (finito--isbn-request-plist (alist-get 'isbn book))
        (format "Successfully deleted info held about '%s'"
                (alist-get 'title book)))))))

(provide 'finito)
;;; finito.el ends here
