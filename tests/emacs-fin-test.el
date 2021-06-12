;;; test-ox-yaow.el --- Tests for emacs-fin.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for emacs-fin.el

;;; Code:

(require 'buttercup)
(require 'emacs-fin)
(require 'cl-lib)

(describe "some test"
  (it "test foo"
    (expect (1+ 1) :to-equal 2)))

(defmacro in-modified-environment (printed-vars body)
  )

(describe "fin-insert-book-data"
  (before-each
    (spy-on 'insert :and-call-fake #'identity))
  (it "test inserted data is reasonable"
    (cl-letf (((symbol-function 'overlay-put) #'ignore))
      (fin-insert-book-data
       '((title . "Flowers for Algernon")
         (author . "Daniel Keyes")
         (description . "A description.")
         (image-file-name . "/some/random/image.png")))
      (expect
       (downcase
        (mapconcat #'spy-context-return-value (spy-calls-all 'insert) ""))
       :to-match
       "flowers for algernon"))))
