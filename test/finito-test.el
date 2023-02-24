;;; finito-test.el --- Tests for finito.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for finito.el

;;; Code:

(require 'buttercup)
(require 'cl-lib)
(require 'dash)

(require 'finito)

(defvar finito--stub-book
  '((title . "Foo Title")
    (authors . ["bar"])
    (description . "foo description")
    (isbn . "isbn")
    (img-uri . "https://random-url")
    (image-file-name . "cache/directory/footitleisbn.jpeg")
    (rating . nil)
    (started-reading . nil)
    (last-read . nil)))

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
  (it "plist has headers and data"
    (let ((plist (finito--search-request-plist "foo" "bar")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--isbn-request-plist"
  (it "plist has headers and data"
    (let ((plist (finito--isbn-request-plist "isbn")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--collection-request-plist"
  (it "plist has headers and data"
    (let ((plist (finito--collection-request-plist "foo")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--collections-request-plist"
  (it "plist has headers and data"
    (let ((plist (finito--collections-request-plist)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--create-collection-request-plist"
  (it "plist has headers and data"
    (let ((plist (finito--create-collection-request-plist "name")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--delete-collection-request-plist"
  (it "plist has headers and data"
    (let ((plist (finito--delete-collection-request-plist "name")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--update-collection-request-plist"
  (it "plist has headers and data"
    (let ((plist (finito--update-collection-request-plist
                  "name"
                  "new name"
                  "dateAdded"
                  'true)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data))))

  (it "error when invalid value passed for sort-ascending"
    (expect (finito--update-collection-request-plist
             "name"
             "new name"
             "dateAdded"
             'not-a-valid-value)
            :to-throw)))

(describe "finito--add-book-request-plist"
  (it "plist has headers and data"
    (let* ((book finito--stub-book)
           (plist (finito--add-book-request-plist book "name")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--remove-book-request-plist"
  (it "plist has headers and data"
    (let* ((plist (finito--remove-book-request-plist "collection" "isbn")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--rate-book-request-plist"
  (it "plist has headers and data"
    (let* ((book finito--stub-book)
           (plist (finito--rate-book-request-plist book 5)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--delete-book-data-request-plist"
  (it "plist has headers and data"
    (let ((plist (finito--delete-book-data-request-plist "isbn")))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--start-reading-request-plist"
  :var ((book finito--stub-book))
  (it "plist has headers and data with no date"
    (let ((plist (finito--start-reading-request-plist book nil)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data))))
  (it "plist has headers and data specified date"
    (let* ((date "2021-03-12")
           (plist (finito--start-reading-request-plist book date)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--finish-reading-request-plist"
  :var ((book finito--stub-book))
  (it "plist has headers and data with no date"
    (let ((plist (finito--finish-reading-request-plist book nil)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data))))
  (it "plist has headers and data specified date"
    (let* ((date "2021-03-12")
           (plist (finito--finish-reading-request-plist book date)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--create-book-request-plist"
  (it "plist has headers and data"
    (let* ((book finito--stub-book)
           (plist (finito--create-book-request-plist book)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--series-request-plist"
  (it "plist has headers and data"
    (let* ((book finito--stub-book)
           (plist (finito--series-request-plist book)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))
 
(describe "finito--summary-request-plist"
  (it "plist has headers and data"
    (let ((plist (finito--summary-request-plist
                  "2021-01-01"
                  "2021-12-29"
                  finito-montage-image-columns
                  finito--montage-large-image-width
                  finito--montage-large-image-height
                  finito--montage-large-image-scale-factor
                  finito-montage-large-image-rating-threshold
                  finito-summary-include-added)))
      (expect (plist-get plist :headers))
      (expect (plist-get plist :data)))))

(describe "finito--insert-book-data"
  :var ((writer (finito-book-writer)))
  (it "inserted data is reasonable"
    (finito--in-buffer
     (finito-insert-book writer
                         '((title . "Flowers for Algernon")
                           (authors . ["Daniel Keyes"])
                           (description . "A description.")
                           (image-file-name . "/some/random/image.png")))
     (expect (downcase .buffer-text) :to-match "flowers for algernon"))))

(describe "finito--create-book-alist"
  (it "book alist contains all keys with correct values"
    (let ((finito-img-cache-directory "cache/directory")
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

  (it "book alist contains keys set to nil when input key is nil"
    (let ((finito-img-cache-directory "cache/directory")
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
              finito--stub-book))))

(describe "finito--book-at-point"
  :var ((test-ewoc (let ((test-ewoc (ewoc-create #'identity)))
                     (ewoc-enter-first test-ewoc 'book-one)
                     (ewoc-enter-first test-ewoc 'book-one)
                     (ewoc-enter-first test-ewoc 'book-one)
                     test-ewoc)))
  (it "returns book on line"
    (cl-letf (((symbol-function 'ewoc-locate)
               (-const (ewoc-nth test-ewoc 0)))
              (finito--ewoc test-ewoc))
      (expect (finito--book-at-point) :to-equal 'book-one))))

(describe "finito--select-collection"
  (it "gets collections and prompts the user"
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "my collection"))
              ((symbol-function 'finito--make-request)
               (lambda (plist callback &rest _) (funcall callback nil))))
      (expect (finito--select-collection #'identity) :to-equal "my collection"))))

(describe "finito--replace-book-at-point-from-request"
  :var* ((book1-alist
          '((title . "GNU Emacs Pocket Reference")
            (authors . ["Debra Cameron"])
            (description . "GNU Emacs is the most popular and widespread of the Emacs family of editors. It is also the most powerful and flexible. Unlike all other text editors, GNU Emacs is a complete working environment -- you can stay within Emacs all day without leaving. The GNU Emacs Pocket Reference is a companion volume to O'Reilly's Learning GNU Emacs, which tells you how to get started with the GNU Emacs editor and, as you become more proficient, it will help you learn how to use Emacs more effectively. This small book, covering Emacs version 20, is a handy reference guide to the basic elements of this powerful editor, presenting the Emacs commands in an easy-to-use tabular format.")
            (isbn . "isbn-book-1")
            (img-uri . "img.jpeg")
            (image-file-name . "cache/directory/foo.jpeg")))

         (book2-alist
          '((title . "Mountains Of The Mind")
            (authors . ["Robert Macfarlane"])
            (description . "WINNER OF THE GUARDIAN FIRST BOOK AWARD Once we thought monsters lived there. In the Enlightenment we scaled them to commune with the sublime. Soon, we were racing to conquer their summits in the name of national pride. In this ground-breaking, classic work, Robert Macfarlane takes us up into the mountains: to experience their shattering beauty, the fear and risk of adventure, and to explore the strange impulses that have for centuries lead us to the world's highest places.")
            (isbn . "isbn-book-2")
            (img-uri . "img.jpeg")
            (image-file-name . "cache/directory/foo2.jpeg")))

         (response-alist
          '((title . "A Random Book")
            (authors . ["???"])
            (description . "GNU Emacs is awesome!")
            (isbn . "a random isbn")
            (thumbnailUri . "some-thumbnail")
            (startedReading . nil)
            (lastRead . nil)))
         
         (expected-buf-string
          "* My Books

** GNU Emacs Pocket Reference

[[cache/directory/foo.jpeg]]  Debra Cameron

GNU Emacs is the most popular and widespread of the Emacs family of editors. It is also the most powerful and flexible. Unlike all other text editors, GNU Emacs is a complete working environment -- you can stay within Emacs all day without leaving. The GNU Emacs Pocket Reference is a companion volume to O'Reilly's Learning GNU Emacs, which tells you how to get started with the GNU Emacs editor and, as you become more proficient, it will help you learn how to use Emacs more effectively. This small book, covering Emacs version 20, is a handy reference guide to the basic elements of this powerful editor, presenting the Emacs commands in an easy-to-use tabular format.

** A Random Book

[[finito/arandombooka random isbn.jpeg]]  ???

GNU Emacs is awesome!


"))
  
  (it "book replaced correctly"
    (cl-letf (((symbol-function 'finito--make-request)
               (lambda (plist callback) (funcall callback response-alist)))
              (finito-img-cache-directory "finito/"))
      (finito--in-buffer
       (let ((finito--ewoc
              (ewoc-create
               (lambda (obj) (finito-insert-book finito-writer-instance obj))
               (format "* My Books\n")))
             (finito--collection "my collection"))
         (ewoc-enter-last finito--ewoc book1-alist)
         (ewoc-enter-last finito--ewoc book2-alist)
         (ewoc-refresh finito--ewoc)
         (goto-char (point-max))
         (finito--replace-book-at-point-from-request nil)
         (expect .buffer-text :to-equal expected-buf-string))))))

(describe "finito--wait-for-server"
  (it "errors when health check expires"
    (cl-letf (((symbol-function 'async-start)
               (lambda (f1 f2) (funcall f1) (funcall f2 nil)))
              ((symbol-function 'finito-start-server-if-not-already) (-const t))
              ((symbol-function 'url-retrieve-synchronously) #'ignore)
              ((symbol-function 'sleep-for) #'ignore))
      (expect (finito--wait-for-server #'ignore) :to-throw)))
  (it "no error when health check succeeds"
    (cl-letf (((symbol-function 'async-start)
               (lambda (f1 f2) (funcall f1) (funcall f2 t)))
              ((symbol-function 'finito-start-server-if-not-already) (-const t))
              ((symbol-function 'url-retrieve-synchronously) (-const t))
              ((symbol-function 'sleep-for) #'ignore))
      (expect (finito--wait-for-server #'ignore) :not :to-throw))))

(describe "finito--health-check"
  (it "returns nil when no server up"
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (error "?"))))
      (expect (finito--health-check) :to-be nil)))
  (it "returns t when url-retrieve-synchronously returns buffer"
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (generate-new-buffer "finito test"))))
      (expect (finito--health-check) :to-be t))))

(describe "finito-start-server-if-not-already"
  (it "errors when jar not found"
    (cl-letf (((symbol-function 'f-exists-p) #'ignore))
      (expect (finito-start-server-if-not-already) :to-throw)))
  (it "returns nil when finito--server-process is running"
    (cl-letf (((symbol-function 'f-exists-p) (-const t))
              ((symbol-function 'process-live-p)
               (lambda (p) (eq p 'proc)))
              (finito--server-process 'proc))
      (expect (finito-start-server-if-not-already) :to-be nil)))
  (it "returns nil when health check succeeds"
    (cl-letf (((symbol-function 'f-exists-p) (-const t))
              ((symbol-function 'finito--health-check) (-const t)))
      (expect (finito-start-server-if-not-already) :to-be nil)))
  (it "returns a process when a server is started"
    (cl-letf (((symbol-function 'f-exists-p) (-const t))
              ((symbol-function 'finito--health-check) #'ignore)
              ((symbol-function 'start-process-shell-command) (-const 'proc)))
      (expect (finito-start-server-if-not-already) :to-be 'proc))))

(describe "finito--seq-to-json-list"
  (it "list with one element"
    (expect (finito--seq-to-json-list '("one"))
            :to-equal
            "[\"one\"]"))

  (it "list with multiple elements"
    (expect (finito--seq-to-json-list '("one" "two"))
            :to-equal
            "[\"one\", \"two\"]")))

(describe "finito-search-for-books"
  :var ((title "my-title")
        (author "my-author")
        (max-results 33))
  (before-each
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (spy-on 'finito--process-books-data :and-return-value nil)
    (spy-on 'finito--search-request-plist :and-call-through)
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback))))

  (it "search for books"
    (finito-search-for-books title author max-results)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect 'finito--process-books-data :to-have-been-called-times 1)
    (expect 'finito--search-request-plist :to-have-been-called-times 1)
    (expect 'finito--wait-for-server :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito--search-request-plist 0)
            :to-equal
            (list title author max-results))))

(describe "finito-create-collection"
  :var ((collection "my collection"))
  (before-each
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))

    (spy-on 'finito--create-collection-request-plist :and-call-through)
    (spy-on 'read-string :and-return-value collection))
  
  (it "creates collection"
    (finito-create-collection)
    (expect 'read-string :to-have-been-called-times 1)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect 'finito--wait-for-server :to-have-been-called-times 1)
    (expect 'finito--create-collection-request-plist :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito--create-collection-request-plist 0)
            :to-equal
            (list collection))))

(describe "finito-open-collection"
  :var ((collection "my collection"))
  (before-each
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (spy-on 'finito--select-collection :and-call-fake
            (lambda (callback) (funcall callback collection)))
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))

    (spy-on 'finito--collection-request-plist :and-call-through)
    (spy-on 'finito--process-books-data :and-return-value nil))

  (it "opens collection"
    (finito-open-collection)
    (expect 'finito--wait-for-server :to-have-been-called-times 1)
    (expect 'finito--select-collection :to-have-been-called-times 1)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect 'finito--collection-request-plist :to-have-been-called-times 1)
    (expect 'finito--process-books-data :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito--collection-request-plist 0)
            :to-equal
            (list collection finito-collection-books-limit 0))))

(describe "finito-open-my-books-collection"
  (before-each
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))

    (spy-on 'finito--collection-request-plist :and-call-through)
    (spy-on 'finito--process-books-data :and-return-value nil))

  (it "opens my books collection"
    (finito-open-my-books-collection)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect 'finito--wait-for-server :to-have-been-called-times 1)
    (expect 'finito--collection-request-plist :to-have-been-called-times 1)
    (expect 'finito--process-books-data :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito--collection-request-plist 0)
            :to-equal
            (list finito-my-books-collection finito-collection-books-limit 0))))

(describe "finito-open-currently-reading-collection"
  (before-each
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))

    (spy-on 'finito--collection-request-plist :and-call-through)
    (spy-on 'finito--process-books-data :and-return-value nil))

  (it "opens currently reading collection"
    (finito-open-currently-reading-collection)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect 'finito--wait-for-server :to-have-been-called-times 1)
    (expect 'finito--collection-request-plist :to-have-been-called-times 1)
    (expect 'finito--process-books-data :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito--collection-request-plist 0)
            :to-equal
            (list finito-currently-reading-collection
                  finito-collection-books-limit
                  0))))

(describe "finito-delete-collection"
  :var ((collection "collection to delete"))
  (before-each
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (spy-on 'finito--select-collection :and-call-fake
            (lambda (callback) (funcall callback collection)))
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))
    
    (spy-on 'finito--delete-collection-request-plist :and-call-through))

  (it "deletes collection"
    (finito-delete-collection)
    (expect 'finito--wait-for-server :to-have-been-called-times 1)
    (expect 'finito--select-collection :to-have-been-called-times 1)
    (expect 'finito--delete-collection-request-plist :to-have-been-called-times 1)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito--delete-collection-request-plist 0)
            :to-equal
            (list collection))))

(describe "finito-update-collection-request"
  :var ((args '(:name "old name"
                      :new-name "new name"
                      :sort "title"
                      :sort-ascending "ascending")))
  (before-each
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))

    (spy-on 'finito--update-collection-request-plist :and-call-through))

  (it "updates collection"
    (finito-update-collection-request args)
    (expect 'finito--wait-for-server :to-have-been-called-times 1)
    (expect 'finito--update-collection-request-plist :to-have-been-called-times 1)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito--update-collection-request-plist 0)
            :to-equal
            (list (plist-get args :name)
                  (plist-get args :new-name)
                  (plist-get args :sort)
                  'true))))

(describe "finito-same-author"
  :var ((book finito--stub-book))
  (before-each
    (spy-on 'finito--book-at-point :and-return-value book)
    (spy-on 'finito-search-for-books :and-return-value nil))

  (it "searches for same author"
    (finito-same-author)
    (expect 'finito--book-at-point :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito-search-for-books 0)
            :to-equal
            (list nil (elt (alist-get 'authors book) 0)))))

(describe "finito-add-book-at-point"
  :var ((collection "collection to add to")
        (book finito--stub-book))
  (before-each
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (spy-on 'finito--select-collection :and-call-fake
            (lambda (callback _) (funcall callback collection)))
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))
    (spy-on 'finito--book-at-point :and-return-value book)
    (spy-on 'finito--add-book-request-plist :and-call-through))

  (it "adds book"
    (finito-add-book-at-point)
    (expect 'finito--book-at-point :to-have-been-called-times 1)
    (expect 'finito--select-collection :to-have-been-called-times 1)
    (expect 'finito--add-book-request-plist :to-have-been-called-times 1)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito--add-book-request-plist 0)
            :to-equal
            (list book collection))))

(describe "finito-add-to-default-book-at-point"
  :var ((book finito--stub-book))
  (before-each
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))
    (spy-on 'finito--book-at-point :and-return-value book)
    (spy-on 'finito--add-book-request-plist :and-call-through))

  (it "adds book"
    (finito-add-to-default-book-at-point)
    (expect 'finito--book-at-point :to-have-been-called-times 1)
    (expect 'finito--add-book-request-plist :to-have-been-called-times 1)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito--add-book-request-plist 0)
            :to-equal
            (list book))))


(describe "finito-remove-book-at-point"
  :var ((book finito--stub-book))
  (before-each
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))
    (spy-on 'finito--make-request :and-return-value nil)
    (spy-on 'finito--book-at-point :and-return-value book)
    (spy-on 'finito--remove-book-request-plist :and-call-through))

  (it "removes book"
    (let ((finito--collection "collection to remove from"))
      (finito-remove-book-at-point)
      (expect 'finito--book-at-point :to-have-been-called-times 1)
      (expect 'finito--remove-book-request-plist :to-have-been-called-times 1)
      (expect 'finito--make-request :to-have-been-called-times 1)
      (expect (spy-calls-args-for 'finito--remove-book-request-plist 0)
              :to-equal
              (list finito--collection (alist-get 'isbn book))))))

(describe "finito-collection-revert"
  (before-each
    (spy-on 'finito--open-specified-collection :and-return-value nil)
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback))))

  (it "refreshes collection"
    (let ((finito--collection "collection to refresh"))
      (finito-collection-revert)
      (expect 'finito--open-specified-collection :to-have-been-called-times 1)
      (expect (car (spy-calls-args-for 'finito--open-specified-collection 0))
              :to-equal
              finito--collection))))

(describe "finito-rate-book-at-point"
  :var ((book finito--stub-book)
        (rating "5"))
  (before-each
    (spy-on 'read-string :and-return-value rating)
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))
    (spy-on 'finito--book-at-point :and-return-value book)
    (spy-on 'finito--rate-book-request-plist :and-call-through)
    (spy-on 'finito--replace-book-at-point-from-request
            :and-return-value nil))

  (it "rates book"
    (let ((finito--collection "collection to refresh"))
      (finito-rate-book-at-point)
      (expect 'finito--book-at-point :to-have-been-called-times 1)
      (expect 'read-string :to-have-been-called-times 1)
      (expect 'finito--replace-book-at-point-from-request
              :to-have-been-called-times 1)
      (expect (spy-calls-args-for 'finito--rate-book-request-plist 0)
              :to-equal
              (list book rating)))))

(describe "finito-start-book-at-point"
  :var ((book finito--stub-book))
  (before-each
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))
    (spy-on 'finito--book-at-point :and-return-value book)
    (spy-on 'finito--start-reading-request-plist :and-call-through)
    (spy-on 'finito--replace-book-at-point-from-request
            :and-return-value nil))

  (it "starts book"
    (let ((finito--collection "collection to refresh"))
      (finito-start-book-at-point)
      (expect 'finito--book-at-point :to-have-been-called-times 1)
      (expect 'finito--replace-book-at-point-from-request
              :to-have-been-called-times 1)
      (expect (spy-calls-args-for 'finito--start-reading-request-plist 0)
              :to-equal
              (list book nil)))))

(describe "finito-finish-book-at-point"
  :var ((book finito--stub-book))
  (before-each
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))
    (spy-on 'finito--book-at-point :and-return-value book)
    (spy-on 'finito--finish-reading-request-plist :and-call-through)
    (spy-on 'finito--replace-book-at-point-from-request
            :and-return-value nil))

  (it "finishes book"
    (let ((finito--collection "collection to refresh"))
      (finito-finish-book-at-point)
      (expect 'finito--book-at-point :to-have-been-called-times 1)
      (expect 'finito--replace-book-at-point-from-request
              :to-have-been-called-times 1)
      (expect (spy-calls-args-for 'finito--finish-reading-request-plist 0)
              :to-equal
              (list book nil)))))

(describe "finito-delete-data-for-book-at-point"
  :var ((book finito--stub-book))
  (before-each
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))
    (spy-on 'finito--book-at-point :and-return-value book)
    (spy-on 'finito--delete-book-data-request-plist :and-call-through))

  (it "finishes book"
    (let ((finito--collection "collection to refresh"))
      (finito-delete-data-for-book-at-point)
      (expect 'finito--book-at-point :to-have-been-called-times 1)
      (expect (spy-calls-args-for 'finito--delete-book-data-request-plist 0)
              :to-equal
              (list (alist-get 'isbn book))))))

(describe "finito-search-request-curl-dbg"
  :var ((author "my author")
        (title "my title")
        (isbn "978-1-56619-909-4"))

  (it "throws error when title, author and isbn not specified"
    (expect (finito-search-request-curl-dbg '(:max-results 3434))
            :to-throw))

  (it "Copies url to kill ring for keyword search"
    (finito-search-request-curl-dbg `(:author ,author :title ,title))
    (let ((copied (car kill-ring)))
      (expect copied
              :to-match
              (rx (* any) (literal (url-hexify-string author)) (* any)))
      (expect copied
              :to-match
              (rx (* any) (literal (url-hexify-string title)) (* any)))))

  (it "Copies url to kill ring for isbn search"
    (finito-search-request-curl-dbg `(:isbn ,isbn))
    (let ((copied (car kill-ring)))
      (expect copied
              :to-match
              (rx (* any) (literal (url-hexify-string isbn)) (* any))))))

(describe "finito-create-book"
  :var ((book (let-alist finito--stub-book
                `((title . ,.title)
                  (authors . ,(append .authors nil))
                  (description . ,.description)
                  (img-uri . ,.img-uri)
                  (isbn . ,.isbn)))))
  (before-each
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))
    (spy-on 'finito--create-book-request-plist :and-call-through)
    (spy-on 'finito--add-book-request-plist :and-call-through))

  (it "creates book"
    (let-alist book
      (finito-create-book
       .title
       (s-join "," .authors)
       .description
       .img-uri
       .isbn)
      (expect 'finito--wait-for-server :to-have-been-called-times 1)
      (expect 'finito--create-book-request-plist :to-have-been-called-times 1)
      (expect 'finito--make-request :to-have-been-called)
      (expect (spy-calls-args-for 'finito--create-book-request-plist 0)
              :to-equal
              (list book))))

  (it "adds created book to default collection"
    (let-alist book
      (finito-create-book
       .title
       (s-join "," .authors)
       .description
       .img-uri
       .isbn)
      (expect 'finito--wait-for-server :to-have-been-called-times 1)
      (expect 'finito--create-book-request-plist :to-have-been-called-times 1)
      (expect 'finito--add-book-request-plist :to-have-been-called-times 1)
      (expect 'finito--make-request :to-have-been-called-times 2)
      (expect (spy-calls-args-for 'finito--add-book-request-plist 0)
              :to-equal
              (list book finito-my-books-collection)))))

(describe "finito-insert-book"
  :var ((book finito--stub-book))
  (it "writes image file if finito-use-image-uris is nil"
    (let ((finito-use-image-uris nil))
      (finito--in-buffer
       (finito-insert-book finito-writer-instance book)
       (expect .buffer-text
               :to-match
               "[[cache/directory/footitleisbn.jpeg]]"))))

  (it "writes image uri if finito-use-image-uris is t"
    (let ((finito-use-image-uris t))
      (finito--in-buffer
       (finito-insert-book finito-writer-instance book)
       (expect .buffer-text
               :to-match
               "[[https://random-url]]")))))

(describe "finito-series-at-point"
  :var ((book finito--stub-book)
        (response-alist
          '((title . "A Random Book")
            (authors . ["???"])
            (description . "GNU Emacs is awesome!")
            (isbn . "a random isbn")
            (thumbnailUri . "some-thumbnail")
            (startedReading . nil)
            (lastRead . nil))))
  
  (before-each
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _) (funcall callback)))
    (spy-on 'finito--book-at-point :and-return-value book)
    (spy-on 'finito--series-request-plist :and-call-through)
    (spy-on 'finito--process-books-data :and-return-value nil))

  (it "open same series"
    (spy-on 'finito--make-request
            :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback (vector response-alist))))
    (finito-series-at-point)
    (expect 'finito--wait-for-server :to-have-been-called-times 1)
    (expect 'finito--series-request-plist :to-have-been-called-times 1)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect 'finito--process-books-data :to-have-been-called-times 1)
    (expect (spy-calls-args-for 'finito--series-request-plist 0)
            :to-equal
            (list book)))

  (it "no new buffer opened if book not in a series"
    (spy-on 'finito--make-request :and-call-fake
            (lambda (plist callback &rest _)
              (funcall callback nil)))
    (finito-series-at-point)
    (expect 'finito--wait-for-server :to-have-been-called-times 1)
    (expect 'finito--series-request-plist :to-have-been-called-times 1)
    (expect 'finito--make-request :to-have-been-called-times 1)
    (expect 'finito--process-books-data :not :to-have-been-called)
    (expect (spy-calls-args-for 'finito--series-request-plist 0)
            :to-equal
            (list book))))

(describe "finito-title-of-book-at-point"
  :var ((book finito--stub-book))
  (before-each
    (spy-on 'finito--wait-for-server :and-call-fake
            (lambda (callback &rest _)
              (funcall callback)))
    (spy-on 'finito--book-at-point :and-return-value book))

  (it "Copies to kill ring"
    (finito-title-of-book-at-point)
    (let ((copied (car kill-ring)))
      (expect 'finito--wait-for-server :to-have-been-called-times 1)
      (expect 'finito--book-at-point :to-have-been-called-times 1)
      (expect copied
              :to-equal
              (alist-get 'title book)))))

(xdescribe "finito-toggle-show-descriptions"
  (it "toggles shown descriptions"
    (let ((finito-use-image-uris t)
          (init-obj (finito-collection-buffer-info
                     :title "collection"
                     :mode #'finito-collection-view-mode
                     :buf-name (concat "Collection ")
                     :buf-name-unique t
                     :books-offset 0
                     :total-books 1))
          (data '[((title . "book") (authors . ["author"]) (description . "description") (isbn . "isbn") (thumbnailUri . "uri") (rating) (startedReading) (lastRead))]))
      ;; TODO why is this necessary?
      (spy-on 'finito-collection-revert :and-return-value nil)
      (finito--process-books-data data init-obj)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-match
              "description")
      (finito-toggle-show-descriptions)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :not
              :to-match
              "description")
      (finito-toggle-show-descriptions)
      (expect (buffer-substring-no-properties (point-min) (point-max))
              :to-match
              "description"))))
