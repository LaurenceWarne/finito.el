;;; emacs-fin.el --- View books -*- lexical-binding: t -*-

;; Copyright (C) 2021 Laurence Warne

;; Author: Laurence Warne
;; Maintainer: Laurence Warne
;; Version: 0.1
;; Keywords: books
;; URL: https://github.com/LaurenceWarne/emacs-fin
;; Package-Requires: ((emacs "27") (dash "2.17.0") (cl-lib "0.3") (request "0.3.2") (f "0.2.0") (s "1.12.0"))

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
;; An Emacs client for fin.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'org)
(require 'request)
(require 's)

(defgroup emacs-fin nil
  "Emacs client to fin"
  :group 'books)

(defcustom fin-insert-book-data
  #'fin--insert-book-data
  "A function which takes book data in the form of an alist, should process and insert it into the current buffer in some way, and then return the lines which contain the content."
  :group 'emacs-fin
  :type 'function)

(defface emacs-fin-author-name
  '((t :foreground "aquamarine"
       :weight bold
       :underline t))
  "Face for author names."
  :group 'emacs-fin)

(defface emacs-fin-book-descriptions
  '((t :italic t))
  "Face for book descriptions."
  :group 'emacs-fin)

(defvar host-uri "http://localhost:8080/api/graphql")

(defun fin-get-data ()
  "Make a request for book data to `host-uri' and insert the contents into a new buffer after completion."
  (request host-uri
    :headers '(("Content-Type" . "application/json")
               ("Accept" . "application/json"))
    :data "{\"query\":\"\\nquery {\\n  books(titleKeywords: \\\"harry\\\", authorKeywords: \\\"rowling\\\") {\\n    title\\n    author\\n    description\\n    isbn\\n    thumbnailUri\\n  }\\n}\"}"
    :parser 'json-read
    :error
    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
    	           (message "Got error: %S" error-thrown)))
    :success (cl-function
    	      (lambda (&key data &allow-other-keys)
                (fin-process-data (cdadar data))))))

(defun fin-process-data (data)
  "Output the book data DATA in a buffer."
  (switch-to-buffer "Books")
  (org-mode)
  (insert "* Books\n\n")
  ;; Vector to list)
  (-each (append data nil)
    (lambda (book)
      (let* ((title (s-replace " " "-" (downcase (alist-get 'title book))))
             (isbn (alist-get 'isbn book))
             (img-uri (alist-get 'thumbnailUri book))
             (image-file-name
              (f-join user-emacs-directory
                      "fin-images/"
                      (concat title isbn ".jpeg")))
             (appended-alist `((image-file-name . ,image-file-name) . ,book)))
        (if (f-exists-p image-file-name)
            (fin-insert-book-data appended-alist)
          (print (concat "Retrieving img: " img-uri))
          ;; this is already a callback so do we need to:
          ;; https://stackoverflow.com/questions/40504796/asynchrous-copy-file-and-copy-directory-in-emacs-lisp
          (url-copy-file img-uri image-file-name)
          (funcall fin-insert-book-data appended-alist)))))
  (goto-char (point-min))
  ;; TODO should not be toggle, should be show
  (org-toggle-inline-images))

(defmacro fin--execute-and-return-start-and-end-lines (body)
  "Execute BODY and return the position of the cursor in the current buffer before BODY was executed, and the position after it was executed."
  `(let ((start (line-number-at-pos)))
     ,body
     (list start (line-number-at-pos))))

(defun fin--insert-book-data (book-data-alist)
  "Insert into the current buffer contents from BOOK-DATA-ALIST."
  (fin--execute-and-return-start-and-end-lines
   (let ((title (alist-get 'title book-data-alist))
         (author (alist-get 'author book-data-alist))
         (description (alist-get 'description book-data-alist))
         (image-file-name (alist-get 'image-file-name book-data-alist)))
     (insert (concat "** " title "\n\n"))
     (insert (concat "[[" image-file-name "]]  " author "\n\n"))
     (overlay-put (make-overlay (- (point) 2) (- (point) (length author) 2))
                  'face
                  'emacs-fin-author-name)
     (insert (concat description "\n\n"))
     (overlay-put (make-overlay (- (point) 2) (- (point) (length description) 2))
                  'face
                  'emacs-fin-book-descriptions))))

;; (fin-get-data)
;; (fin-process-data '(((title . "Flowers for Algernon") (author . "Daniel Keyes") (description . "'A masterpiece of poignant brilliance . . . heartbreaking' Guardian Charlie Gordon, a floor sweeper born with an unusually low IQ, has been chosen as the perfect subject for an experimental surgery that doctors hope will increase his intelligence - a procedure that has been highly successful when tested on a lab mouse named Algernon. All Charlie wants is to be smart and have friends, but the treatement turns him into a genius. Then Algernon begins to fade. What will become of Charlie?") (thumbnailUri . "http://books.google.com/books/content?id=VbOtAQAACAAJ&printsec=frontcover&img=1&zoom=1&source=gbs_api"))))

(provide 'emacs-fin)
;;; emacs-fin.el ends here
