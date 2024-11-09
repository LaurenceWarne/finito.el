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
    (let* ((request-backend 'url-retrieve)
           (uri (concat finito--download-url "v" finito-server-version "/" finito--jar-name)))
      (make-directory finito-server-directory t)
      (message "Downloading server jar from %s to %s" uri finito--server-path)
      (url-copy-file uri finito--server-path))))

(describe "integration"
  :var* ((search-buffer-string "Book Search")
         (my-books-buffer-string "Collection: My Books"))

  (before-all
    (let* ((dir (f-expand default-directory))
           (finito-test-config-home (f-join dir "/tmp/finito-test-config-home"))
           (finito-test-data-home (f-join dir "/tmp/finito-test-data-home"))
           (conf-path (f-join finito-test-config-home "libro-finito" "service.conf"))
           (db-path (f-join finito-test-data-home "libro-finito" "db.sqlite"))
           (finito--base-uri finito--test-uri))
      (with-environment-variables (("XDG_CONFIG_HOME" finito-test-config-home)
                                   ("XDG_DATA_HOME" finito-test-data-home))
        (make-directory (f-parent conf-path) t)
        (ignore-errors (f-delete db-path) (f-delete conf-path))
        (f-touch conf-path)
        (f-write-text (format "port=%s,default-collection=\"My Books\"" finito--test-port) 'utf-8 conf-path)
        (finito--download-sync)
        (finito-start-server-if-not-already)
        (sit-for 5))))

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
