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

(defun lw--fn-from-list (ls)
  (let ((calls 0))
    (lambda (&rest)
      (when (>= calls (length ls))
        (error "Stubbed function called more times than allowed"))
      (cl-incf calls)
      (nth (1- calls) ls))))

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
                   (img-uri . "image.png")))
           (plist (finito--add-book-request-plist "name" book)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--remove-book-request-plist"
  (it "test plist has headers and data"
    (let* ((plist (finito--remove-book-request-plist "collection" "isbn")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--rate-book-request-plist"
  (it "test plist has headers and data"
    (let* ((book '((title . "Flowers for Algernon")
                   (authors . ["Daniel Keyes"])
                   (description . "A description.")
                   (isbn . "740253425430.")
                   (img-uri . "image.png")))
           (plist (finito--rate-book-request-plist book 5)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--delete-book-data-request-plist"
  (it "test plist has headers and data"
    (let ((plist (finito--delete-book-data-request-plist "isbn")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--start-reading-request-plist"
  :var ((book '((title . "Flowers for Algernon")
                (authors . ["Daniel Keyes"])
                (description . "A description.")
                (isbn . "740253425430.")
                (img-uri . "image.png"))))
  (it "test plist has headers and data with no date"
    (let ((plist (finito--start-reading-request-plist book nil)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data))))
  (it "test plist has headers and data specified date"
    (let* ((date "2021-03-12")
           (plist (finito--start-reading-request-plist book date)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--finish-reading-request-plist"
  :var ((book '((title . "Flowers for Algernon")
                (authors . ["Daniel Keyes"])
                (description . "A description.")
                (isbn . "740253425430.")
                (img-uri . "image.png"))))
  (it "test plist has headers and data with no date"
    (let ((plist (finito--finish-reading-request-plist book nil)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data))))
  (it "test plist has headers and data specified date"
    (let* ((date "2021-03-12")
           (plist (finito--finish-reading-request-plist book date)))
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
                            (thumbnailUri . "https://random-url")
                            (rating . 3)
                            (startedReading . "some-date")
                            (lastRead . "some other date"))))
      (expect (finito--create-book-alist response-alist)
              :to-equal
              '((title . "Foo Title")
                (authors . ["bar"])
                (description . "foo description")
                (isbn . "isbn")
                (img-uri . "https://random-url")
                (image-file-name . "cache/directory/footitleisbn.jpeg")
                (rating . 3)
                (started-reading . "some-date")
                (last-read . "some other date")))))

  (it "test book alist contains keys set to nil when input key is nil"
    (let ((finito-image-cache-dir "cache/directory")
          (response-alist '((title . "Foo Title")
                            (authors . ["bar"])
                            (description . "foo description")
                            (isbn . "isbn")
                            (thumbnailUri . "https://random-url")
                            (rating . nil)
                            (startedReading . nil)
                            (lastRead . nil))))
      (expect (finito--create-book-alist response-alist)
              :to-equal
              '((title . "Foo Title")
                (authors . ["bar"])
                (description . "foo description")
                (isbn . "isbn")
                (img-uri . "https://random-url")
                (image-file-name . "cache/directory/footitleisbn.jpeg")
                (rating . nil)
                (started-reading . nil)
                (last-read . nil))))))

(describe "finito--book-at-point"
  :var ((books-alist '((3 . book-one) (4 . book-two) (20 . book-three))))
  (it "test errors when cursor before all books"
    (cl-letf (((symbol-function 'line-number-at-pos) #'ignore)
              (finito--buffer-books books-alist))
      (expect (finito--book-at-point) :to-throw)))
  (it "test returns book on line where it starts"
    (cl-letf (((symbol-function 'line-number-at-pos) (lambda () 3))
              (finito--buffer-books books-alist))
      (expect (finito--book-at-point) :to-equal 'book-one)))
  (it "test returns book on line after it starts"
    (cl-letf (((symbol-function 'line-number-at-pos) (lambda () 15))
              (finito--buffer-books books-alist))
      (expect (finito--book-at-point) :to-equal 'book-two))))

(describe "finito--books-filter"
  :var ((books-alist
         '((3 . book-one) (4 . book-two) (20 . book-four) (15 . book-three))))
  (it "test all positive filter gives list with same elements"
    (let ((alist-response (finito--books-filter (lambda (_) t) books-alist)))
      (expect (--all-p (-contains-p alist-response it) alist-response))))
  (it "test element removed and value correctly subtracted"
    (let ((alist-response (finito--books-filter
                           (lambda (e) (not (eq e 'book-two))) books-alist)))
      (expect (length alist-response) :to-equal 3)
      (expect alist-response :to-contain '(3 . book-one))
      (expect alist-response :to-contain '(4 . book-three))
      (expect alist-response :to-contain '(9 . book-four)))))

(describe "finito--diffs"
  :var ((ls '(3 4 15 20)))
  (it "test diffs are correct"
    (expect (finito--diffs '(3 4 15 20)) :to-equal '(3 1 11 5))))

(describe "finito--select-collection"
  (it "test gets collections and prompts the user"
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "my collection"))
              ((symbol-function 'finito--make-request)
               (lambda (plist callback) (funcall callback nil))))
      (expect (finito--select-collection #'identity) :to-equal "my collection"))))
