;;; test-ox-yaow.el --- Tests for finito.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for finito.el

;;; Code:

(require 'buttercup)
(require 'finito)
(require 'cl-lib)

(defmacro in-stubbed-buffer (inserted-var body &rest expectations)
  )

(describe "finito--insert-book-data"
  (before-each
    (spy-on 'insert :and-call-fake #'identity))
  (it "test inserted data is reasonable"
    (cl-letf (((symbol-function 'overlay-put) #'ignore))
      (finito--insert-book-data
       '((title . "Flowers for Algernon")
         (author . "Daniel Keyes")
         (description . "A description.")
         (image-file-name . "/some/random/image.png")))
      (expect
       (downcase
        (mapconcat #'spy-context-return-value (spy-calls-all 'insert) ""))
       :to-match
       "flowers for algernon"))))

(describe "finito--create-book-alist"
  (it "test book alist contains all keys with correct values"
    (let ((finito-image-cache-dir "cache/directory")
          (response-alist '((title . "Foo Title")
                            (author . "bar")
                            (description . "foo description")
                            (isbn . "isbn")
                            (thumbnailUri . "https://random-url"))))
      (expect (finito--create-book-alist response-alist)
              :to-equal
              '((title . "Foo Title")
                (author . "bar")
                (description . "foo description")
                (isbn . "isbn")
                (img-uri . "https://random-url")
                (image-file-name . "cache/directory/foo-titleisbn.jpeg"))))))
