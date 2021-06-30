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

(require 'finito-graphql)
(require 'finito-view)

(defgroup finito nil
  "Emacs client to finito"
  :group 'books)

(defcustom finito-insert-book-data
  #'finito--insert-book-data
  "Function to insert book data into the current buffer.

The function should take book data in the form of an alist, and insert info
into the current buffer."
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

;;; Buffer local variables

(defvar-local finito--buffer-books
  nil
  "An alist associating books to buffer lines they begin.")

;;; Misc functions

(defun finito--search-request-plist
    (title-keywords author-keywords &optional max-results)
  "Return a plist with headers and body deduced from TITLE-KEYWORDS, AUTHOR-KEYWORDS and MAX-RESULTS."
  (let* ((query-variable-str
          (format finito--search-query-variables
                  (if (> (length title-keywords) 0)
                      (s-wrap title-keywords "\"") "null")
                  (if (> (length author-keywords) 0)
                      (s-wrap  author-keywords "\"") "null")
                  (or max-results "null"))))
    `(:headers
      (("Content-Type" . "application/json")
       ("Accept" . "application/json"))
      :data
      ,(format "{\"query\":\"%s\", \"variables\": %s\}"
               finito--search-query
               query-variable-str))))

(defun finito--isbn-request-plist (isbn)
  "Return a plist with headers and body deduced from ISBN."
  `(:headers
    (("Content-Type" . "application/json")
     ("Accept" . "application/json"))
    :data
    ,(format "{\"query\":\"%s\", \"variables\": %s\}"
             finito--isbn-query
             (format finito--isbn-query-variables isbn))))

(defun finito--make-request (request-plist callback)
  "Make a request to `finito--host-uri' using REQUEST-PLIST.

CALLBACK is called with the parsed json if the request is successful."
  (request finito--host-uri
    :headers (plist-get request-plist :headers)
    :data (plist-get request-plist :data)
    :parser 'json-read
    :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
    	           (message "Got error: %S" error-thrown)))
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
      (funcall finito-insert-book-data book-alist))))

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
      `((title . ,.title)
        (authors . ,.authors)
        (description . ,.description)
        (isbn . ,.isbn)
        (img-uri . ,.thumbnailUri)
        (image-file-name . ,image-file-name)))))

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

;;; Modes

(defvar finito-book-view-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "a" #'finito-add-book)
    (define-key map "c" #'finito-add-to-collection)
    (define-key map "A" #'finito-same-author)
    (define-key map "n" #'outline-next-heading)
    (define-key map "p" #'outline-previous-heading)
    (define-key map "o" #'finito-to-org-buffer)
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

(defun add-book ()
  "Add the book at point."
  (interactive)
  nil)

(defun add-book-to-collection ()
  "Add the book at point to a collection."
  (interactive)
  nil)

(defun finito-to-org-buffer ()
  "Open the current buffer as a normal org mode buffer."
  (interactive)
  nil)

(defun finito-request (&optional args)
  "Send a request to the finito server using transient args ARGS."
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

(provide 'finito)
;;; finito.el ends here
