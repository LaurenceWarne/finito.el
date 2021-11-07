;;; finito-integration.el --- integration tests for finito.el -*- lexical-binding: t -*-

;;; Commentary:

;; Integration tests for finito.el

;;; Code:

(require 'buttercup)

(require 'finito)
(require 'request)
(require 'url)

(defun finito--download-sync ()
  "Download a finito server synchronously."
  (unless (file-exists-p finito--server-path)
    (let ((request-backend 'url-retrieve))
      (make-directory finito-server-directory t)
      (message "Starting finito server download...")
      (thread-first
          (concat finito--download-url "v" finito-server-version)
        (concat "/" finito--jar-name)
        (url-copy-file finito--server-path))
      (message "Finished finito server download"))))

(describe "integration"
  :var ((search-buffer-string "Book Search")
        (my-books-buffer-string "Collection: My Books"))

  (before-all
    (finito--download-sync))

  (it "Search, add book, open collection"
    (finito-search-for-books "Emacs" nil)
    (with-timeout (60 ((lambda () (buttercup-fail "Timeout on search"))))
      (while (not (get-buffer search-buffer-string))
        (sleep-for 0.1)))
    (switch-to-buffer (get-buffer search-buffer-string))
    (outline-next-heading)
    (finito-add-to-default-book-at-point)
    (sleep-for 5)
    (let ((book (finito--book-at-point)))
      (finito-open-my-books-collection)
      (with-timeout (10 ((lambda () (buttercup-fail "Timeout on My Books"))))
        (while (not (get-buffer my-books-buffer-string))
          (sleep-for 0.1)))
      (switch-to-buffer (get-buffer my-books-buffer-string))
      (outline-next-heading)
      (expect book :to-equal (finito--book-at-point)))))
