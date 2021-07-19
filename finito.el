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

(require 'finito-buffer)
(require 'finito-core)
(require 'finito-graphql)
(require 'finito-view)

(eval-when-compile (require 'let-alist))

(defcustom finito-writer-instance
  (finito-book-writer)
  "`finito-book-writer' instance to be used.

This object will be used to write books in finito buffers."
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

(defcustom finito-keyword-search-buffer-init-instance
  (finito-buffer-info :title "Books" :mode (lambda () (finito-search-view-mode)))
  "`finito-buffer-info' instance to be used.

This instance will be used to initialise a buffer after a keyword search."
  :group 'finito
  :type 'object)

;;; Custom variables

(defcustom finito-my-books-collection
  "My Books"
  "The name of the \"default\" collection.

 This collection will be opened when the \"My Books\" suffix is invoked from
the `finito-dispatch' prefix command.  Typically this collection will hold all
books which have been added at some point to some collection."
  :group 'finito
  :type 'string)

(defcustom finito-currently-reading-collection
  "Currently Reading"
  "The name of the collection which holds books being currently read.

 This collection will be opened when the \"Currently Reading\" suffix is
invoked from the `finito-dispatch' prefix command."
  :group 'finito
  :type 'object)

(defconst finito--headers
  '(("Content-Type" . "application/json")
    ("Accept" . "application/json")))

(defvar finito--host-uri "http://localhost:8080/api/graphql")

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

(defun finito--process-books-data (data init-obj)
  "Insert the books data DATA into a buffer.

Use INIT-OBJ, an instance of `finito-buffer-init' to initialize the buffer."
  (finito--process
   data
   init-obj
   ;; Vector to list)
   (##-each (append % nil) #'finito--layout-book-data)))

(defun finito--process-single-book (data init-obj)
  "Insert the book data DATA into a buffer.

Use INIT-OBJ, an instance of `finito-buffer-init' to initialize the buffer."
  (finito--process data init-obj (##finito--layout-book-data %)))

(defun finito--process (data init-obj callback)
  "Set up a finito buffer using INIT-OBJ, then call CALLBACK with DATA."
  (unless (f-dir-p finito-image-cache-dir) (f-mkdir finito-image-cache-dir))
  (switch-to-buffer (generate-new-buffer-name "Books"))
  (finito-init-buffer init-obj)
  (let ((inhibit-read-only t))
    (insert (format "* %s\n\n" (oref init-obj title)))
    (funcall callback data))
  (goto-char (point-min))
  (org-display-inline-images))

(defun finito--layout-book-data (book)
  "Insert data for BOOK into the current buffer."
  (add-to-list 'finito--buffer-books `(,(line-number-at-pos) . ,book))
  (let ((book-alist (finito--create-book-alist book)))
    (let-alist book-alist
      (unless (f-exists-p .image-file-name)
        (message (concat "Retrieving img: " .img-uri))
        ;; TODO this is already a callback so do we need to:
        ;; https://stackoverflow.com/questions/40504796/asynchrous-copy-file-and-copy-directory-in-emacs-lisp
        (url-copy-file .img-uri .image-file-name))
      (finito-insert-book finito-writer-instance book-alist))))

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

(defun finito--book-at-point ()
  "Get the book at the current point in the buffer."
  (when-let* ((line (line-number-at-pos))
              ;; Alist may not be ordered
              (books-before (--filter (<= (car it) line) finito--buffer-books)))
    (cdr (--max-by (> (car it) (car other)) books-before))))

(defun finito--books-filter (pred books)
  "Remove books matching PRED from BOOKS.

BOOKS is expected to be in the format of `finito--buffer-books.'"
  (let ((diffs (finito--diffs (-map #'car books)))
        (sorted-books (--sort (< (car it) (car other)) books)))
    (cdr (-reduce-from
          (lambda (acc book-cons)
            (-let (((index . book) book-cons)
                   ((sub-acc . books-acc) acc))
              (if (funcall pred book)
                  (cons sub-acc (-snoc books-acc (cons (- index sub-acc) book)))
                (let ((diff (nth (-elem-index book-cons sorted-books) diffs)))
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

(defun finito--select-collection (callback)
  "Prompt for a collection, and then call CALLBACK with that collection."
  (finito--make-request
     (finito--collections-request-plist)
     (lambda (response)
       (let* ((all-collections (-map #'cdar response))
              (chosen-collection (completing-read "Choose: " all-collections)))
         (print chosen-collection)
         (funcall callback chosen-collection)))))

(defun finito--browse-function (book-alist)
  "Open an openlibrary page of a book, using the isbn in BOOK-ALIST."
  (browse-url
   (concat "https://openlibrary.org/isbn/" (alist-get 'isbn book-alist))))

(defun finito--open-specified-collection (collection)
  "Open the collection COLLECTION in a view buffer."
  (finito--make-request
   (finito--collection-request-plist collection)
   (##finito--process-books-data
    (cdar %)
    (finito-collection-buffer-info :title collection
                                   :mode #'finito-collection-view-mode))))

;;; Modes

(defvar finito-search-view-mode-map
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
  nil)

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

(defun finito-update-collection-request (&optional args)
  "Send a collection update request to the finito server using ARGS."
  (interactive
   (list (finito--transient-args-plist 'finito-update-collection))))

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
(defun finito-currently-reading (&optional _args)
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

(defun finito-remove-book-at-point ()
  "Remove the book at point from the current collection."
  (interactive)
  (let* ((line (line-number-at-pos))
         (book (finito--book-at-point))
         (isbn (alist-get 'isbn book))
         (books finito--buffer-books))
    (unless book (error "Cursor is not at a book!"))
    (finito--make-request
     (finito--remove-book-request-plist finito--collection isbn)
     (lambda (_)
       (message "Removed '%s' from %s" (alist-get 'title book) finito--collection)
       (let* ((inhibit-read-only t)
              (indices (-sort #'< (-map #'car books)))
              (idx (--find-last-index (<= it line) indices))
              (book-start-line (nth idx indices)))
         ;; There's got to be a better way...
         (goto-char (point-min))
         (forward-line (1- book-start-line))
         (--dotimes (- (or (nth (1+ idx) indices)
                           (line-number-at-pos (point-max)))
                       book-start-line)
           (delete-region (point) (1+ (line-end-position)))))
       (setq finito--buffer-books
             (finito--books-filter (##not (eq (alist-get 'isbn %) isbn))
                                   books))))))

(defun finito-refresh-collection ()
  "Refresh the current collection."
  (interactive)
  (let ((collection finito--collection)
        (old-point (point)))
    (kill-current-buffer)
    ;; TODO make this blocking/synchronous
    (finito--make-request
     (finito--collection-request-plist collection)
     (lambda (data)
       (finito--process-books-data
        (cdar data)
        (finito-collection-buffer-info :title collection
                                       :mode #'finito-collection-view-mode))
       (goto-char (min old-point (point-max)))))))

(defun finito-browse-book-at-point ()
  "Browse the book at point."
  (interactive)
  (funcall finito-browse-function (finito--book-at-point)))

(defun finito-rate-book-at-point ()
  "Rate the book at point."
  (interactive))

(defun finito-start-book-at-point ()
  "Rate the book at point."
  (interactive))

(defun finito-start-and-date-book-at-point ()
  "Rate the book at point."
  (interactive))

(defun finito-finish-book-at-point ()
  "Rate the book at point."
  (interactive))

(defun finito-finish-and-date-book-at-point ()
  "Rate the book at point."
  (interactive))

(provide 'finito)
;;; finito.el ends here
