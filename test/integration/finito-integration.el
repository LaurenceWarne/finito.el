;;; finito-integration.el --- integration tests for finito.el -*- lexical-binding: t -*-

;;; Commentary:

;; Integration tests for finito.el

;;; Code:

(require 'buttercup)

(require 'finito)
(require 'f)
(require 'request)
(require 'url)

(defun finito--download-sync ()
  "Download a finito server synchronously."
  (unless (file-exists-p finito--server-path)
    (let ((request-backend 'url-retrieve))
      (make-directory finito-server-directory t)
      (thread-first
          (concat finito--download-url "v" finito-server-version)
        (concat "/" finito--jar-name)
        (url-copy-file finito--server-path)))))

(describe "integration"
  :var* ((search-buffer-string "Book Search")
         (my-books-buffer-string "Collection: My Books"))

  (before-all
    (let* ((dir (f-expand default-directory))
           (finito-misc (f-join dir ".eldev/finito-misc"))
           (db-path (f-join finito-misc "db.sqlite"))
           (finito-config-directory finito-misc))
      (f-mkdir finito-misc)
      (f-write-text (format "{database-path = %s}" db-path)
                    'utf-8
                    (f-join finito-config-directory "service.conf"))
      (ignore-errors (f-delete db-path))
      (finito--download-sync)
      (finito-start-server-if-not-already)
      (sit-for 5)))

  (it "Search, add book, open collection"
    (finito-search-for-books "Emacs" nil)
    (with-timeout (60 ((lambda () (buttercup-fail "Timeout on search"))))
      (while (not (get-buffer search-buffer-string))
        (sit-for 0.1)))
    (switch-to-buffer (get-buffer search-buffer-string))
    (outline-next-heading)
    (finito-add-to-default-book-at-point)
    (sit-for 5)
    (let ((book (finito--book-at-point)))
      (finito-open-my-books-collection)
      (with-timeout (10 ((lambda () (buttercup-fail "Timeout on My Books"))))
        (while (not (get-buffer my-books-buffer-string))
          (sit-for 0.1)))
      (switch-to-buffer (get-buffer my-books-buffer-string))
      (outline-next-heading)
      (let-alist (finito--book-at-point)
        (expect (alist-get 'title book) :to-equal .title)
        (expect (alist-get 'authors book) :to-equal .authors))))
  
  (it "Search, start book"
    (finito-search-for-books "Emacs" nil)
    (with-timeout (10 ((lambda () (buttercup-fail "Timeout on search"))))
      (while (not (get-buffer search-buffer-string))
        (sit-for 0.1)))
    (switch-to-buffer (get-buffer search-buffer-string))
    (outline-next-heading)
    (finito-start-book-at-point)
    (sit-for 5)
    (let-alist (finito--book-at-point)
      (expect .started-reading :to-be-truthy)))

  (it "Search, finish book"
    (finito-search-for-books "Emacs" nil)
    (with-timeout (10 ((lambda () (buttercup-fail "Timeout on search"))))
      (while (not (get-buffer search-buffer-string))
        (sit-for 0.1)))
    (switch-to-buffer (get-buffer search-buffer-string))
    (outline-next-heading)
    (finito-finish-book-at-point)
    (sit-for 5)
    (let-alist (finito--book-at-point)
      (expect .last-read :to-be-truthy))))
