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

(describe "fin-insert-book-data"
          (it "test inserted data is reasonable"
              (let ((fn-output))
                (cl-letf* (((symbol-function 'insert)
                            (lambda (string) (add-to-list 'fn-output string t)))
                           ((symbol-function 'overlay-put) #'ignore))
                  (fin-insert-book-data
                   '((title . "Flowers for Algernon")
                     (author . "Daniel Keyes")
                     (description . "A description.")
                     (image-file-name . "/some/random/image.png"))))
                (print fn-output)
                (expect fn-output :to-equal '("foo"))
                ;; (expect (cl-search "flowers for algernon"
                ;;                    (downcase (mapconcat #'identity fn-output "\n")))
                ;;         :not :to-be nil)
                )))
