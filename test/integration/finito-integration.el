;;; finito-integration.el --- integration tests for finito.el -*- lexical-binding: t -*-

;;; Commentary:

;; Integration tests for finito.el

;;; Code:

(require 'buttercup)

(require 'finito)
(require 'f)
(require 'request)
(require 'url)

(defvar finito--test-port "24684")
(defvar finito--test-uri (format "http://localhost:%s" finito--test-port))

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
           (finito-config-directory finito-misc)
           (service-path (f-join finito-config-directory "service.conf"))
           (finito--base-uri finito--test-uri))
      (f-mkdir finito-misc)
      (f-write-text (format "database-path = \"%s\"\nport=%s" db-path finito--test-port)
                    'utf-8
                    service-path)
      (ignore-errors (f-delete db-path))
      (finito--download-sync)
      (finito-start-server-if-not-already)
      (sit-for 5)))

  (it "Search, add book, open collection"
    ;; https://github.com/jorgenschaefer/emacs-buttercup/issues/127
    (let ((finito--base-uri finito--test-uri))
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
          (expect (alist-get 'authors book) :to-equal .authors)))))
  
  (it "Search, start book"
    (let ((finito--base-uri finito--test-uri))
      (finito-search-for-books "Emacs" nil)
      (with-timeout (10 ((lambda () (buttercup-fail "Timeout on search"))))
        (while (not (get-buffer search-buffer-string))
          (sit-for 0.1)))
      (switch-to-buffer (get-buffer search-buffer-string))
      (outline-next-heading)
      (finito-start-book-at-point)
      (sit-for 5)
      (let-alist (finito--book-at-point)
        (expect .started-reading :to-be-truthy))))

  (it "Search, finish book"
    (let ((finito--base-uri finito--test-uri))
      (finito-search-for-books "Emacs" nil)
      (with-timeout (10 ((lambda () (buttercup-fail "Timeout on search"))))
        (while (not (get-buffer search-buffer-string))
          (sit-for 0.1)))
      (switch-to-buffer (get-buffer search-buffer-string))
      (outline-next-heading)
      (finito-finish-book-at-point)
      (sit-for 5)
      (let-alist (finito--book-at-point)
        (expect .last-read :to-be-truthy)))))
