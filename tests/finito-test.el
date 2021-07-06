;;; test-ox-yaow.el --- Tests for finito.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for finito.el

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(require 'finito)

(defmacro finito--in-buffer (&rest body)
  "Execute BODY in a throw-away buffer.

Occurrences of `.buffer-text' will be replaced by:
`(buffer-substring-no-properties (point-min) (point-max))'"
  (cl-labels
      ((replace (expr)
                (pcase expr
                  (`(,car . ,cdr)    (cons (replace car) (replace cdr)))
                  ('.buffer-text     '(buffer-substring-no-properties
                                       (point-min) (point-max)))
                  (_                 expr))))
    `(with-temp-buffer
       ,@(replace body))))


(describe "finito--search-request-plist"
  (it "test plist has headers and data"
    (let ((plist (finito--search-request-plist "foo" "bar")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--isbn-request-plist"
  (it "test plist has headers and data"
    (let ((plist (finito--isbn-request-plist "isbn")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--collection-request-plist"
  (it "test plist has headers and data"
    (let ((plist (finito--collection-request-plist "foo")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--collections-request-plist"
  (it "test plist has headers and data"
    (let ((plist (finito--collections-request-plist)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--create-collection-request-plist"
  (it "test plist has headers and data"
    (let ((plist (finito--create-collection-request-plist "name")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--delete-collection-request-plist"
  (it "test plist has headers and data"
    (let ((plist (finito--delete-collection-request-plist "name")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--add-book-request-plist"
  (it "test plist has headers and data"
    (let* ((book '((title . "Flowers for Algernon")
                   (authors . ["Daniel Keyes"])
                   (description . "A description.")
                   (isbn . "740253425430.")
                   (thumbnailUri . "image.png")))
           (plist (finito--add-book-request-plist "name" book)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--insert-book-data"
  :var ((writer (finito-book-writer)))
  (it "test inserted data is reasonable"
    (finito--in-buffer
     (finito-insert-book writer
       '((title . "Flowers for Algernon")
         (authors . ["Daniel Keyes"])
         (description . "A description.")
         (image-file-name . "/some/random/image.png")))
     (expect (downcase .buffer-text) :to-match "flowers for algernon"))))

(describe "finito--create-book-alist"
  (it "test book alist contains all keys with correct values"
    (let ((finito-image-cache-dir "cache/directory")
          (response-alist '((title . "Foo Title")
                            (authors . ["bar"])
                            (description . "foo description")
                            (isbn . "isbn")
                            (thumbnailUri . "https://random-url"))))
      (expect (finito--create-book-alist response-alist)
              :to-equal
              '((title . "Foo Title")
                (authors . ["bar"])
                (description . "foo description")
                (isbn . "isbn")
                (img-uri . "https://random-url")
                (image-file-name . "cache/directory/footitleisbn.jpeg"))))))

(describe "finito--book-at-point"
  :var ((books-alist '((3 . book-one) (4 . book-two) (20 . book-three))))
  (it "test book at point returns nil before all books"
    (cl-letf (((symbol-function 'line-number-at-pos) #'ignore)
              (finito--buffer-books books-alist))
      (expect (finito--book-at-point) :to-be nil)))
  (it "test book at point returns book on line where it starts"
    (cl-letf (((symbol-function 'line-number-at-pos) (lambda () 3))
              (finito--buffer-books books-alist))
      (expect (finito--book-at-point) :to-equal 'book-one)))
  (it "test book at point returns book on line after it starts"
    (cl-letf (((symbol-function 'line-number-at-pos) (lambda () 15))
              (finito--buffer-books books-alist))
      (expect (finito--book-at-point) :to-equal 'book-two))))

