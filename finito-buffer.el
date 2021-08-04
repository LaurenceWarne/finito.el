;;; finito-view.el --- Buffer utilities for finito -*- lexical-binding: t -*-

;; Copyright (C) 2021 Laurence Warne

;; Author: Laurence Warne

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file contains utilities for working with libro finito buffers

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'iso8601)
(require 's)

(require 'finito-core)

;;; Faces

(defface finito-author-name
  '((t :foreground "aquamarine"
       :weight bold
       :underline t))
  "Face for author names."
  :group 'finito)

(defface finito-book-descriptions
  '((t :foreground "grey"))
  "Face for book descriptions."
  :group 'finito)

(defface finito-rating
  '((t :foreground "gold"
       :weight bold))
  "Face for ratings."
  :group 'finito)

(defface finito-currently-reading
  '((t :foreground "sky blue"
       :italic t))
  "Face for currently reading message."
  :group 'finito)

(defface finito-last-read
  '((t :foreground "light green"
       :weight bold))
  "Face for last read message."
  :group 'finito)

;;; Buffer local variables

(defvar-local finito--buffer-books
  nil
  "An alist associating books to buffer lines they begin.

It's elements should be of the form (KEY . VALUE) where KEY is an integer
representing the start of where information starts about a particular book
in the current buffer.  VALUE is itself an alist containing information
about the corresponding book.  There are no guarentees on the orderedness
of this variables.")

(defvar-local finito--collection
  nil
  "The name of the current collection.")

;;; Classes for working with buffers

(defclass finito-book-writer ()
  nil
  "A class for writing book information to a buffer.")

(cl-defmethod finito-insert-book ((writer finito-book-writer) book-alist)
  "Write BOOK-ALIST into the current buffer using WRITER."
  (let-alist book-alist
    (finito-insert-title writer .title)
    (finito-insert-image
     writer
     (let ((display-remote (bound-and-true-p org-display-remote-inline-images)))
       (if (and display-remote (not (eq display-remote 'skip)))
           .img-uri
         .image-file-name)))
    (finito-insert-author writer .authors)
    (when .rating (finito-insert-rating writer .rating))
    (when .started-reading
      (finito-insert-started-reading writer .started-reading))
    (when .last-read
      (finito-insert-last-read writer .last-read))
    (finito-insert-description writer .description)))

(cl-defmethod finito-insert-title ((_writer finito-book-writer) title)
  "Insert TITLE into the current buffer.

_WRITER is a `finito-book-writer', but it's properties have no bearing on the
result of this method."
  (insert (concat "** " title "\n\n")))

(cl-defmethod finito-insert-image ((_writer finito-book-writer) image)
  "Insert IMAGE (an image file name) into the current buffer.

_WRITER is a `finito-book-writer', but it's properties have no bearing on the
result of this method."
  (insert (concat "[[" image "]]  ")))

(cl-defmethod finito-insert-author ((_writer finito-book-writer) authors)
  "Insert AUTHORS into the current buffer.

_WRITER is a `finito-book-writer', but it's properties have no bearing on the
result of this method."
  (let ((authors-str (s-join ", " authors)))
    (insert (concat authors-str "\n\n"))
    (overlay-put (make-overlay (- (point) 2) (- (point) (length authors-str) 2))
                 'face
                 'finito-author-name)))

(cl-defmethod finito-insert-rating ((_writer finito-book-writer) rating)
  "Insert RATING into the current buffer.

_WRITER is a `finito-book-writer', but it's properties have no bearing on the
result of this method."
  (insert
   (concat (-repeat (min rating 100) ?★) "\n"))
  (overlay-put (make-overlay (1- (point)) (- (point) (min rating 100) 1))
                 'face
                 'finito-rating))

(cl-defmethod finito-insert-started-reading
  ((_writer finito-book-writer) _started-reading)
  "Insert STARTED-READING into the current buffer.

_WRITER is a `finito-book-writer', but it's properties have no bearing on the
result of this method."
  (let ((currently-reading-str "⌛ In Progress ⌛"))
    (insert (concat currently-reading-str) "\n")
    (overlay-put (make-overlay (1- (point)) (- (point)
                                               (length currently-reading-str) 1))
                 'face
                 'finito-currently-reading)))

(cl-defmethod finito-insert-last-read
  ((_writer finito-book-writer) last-read)
  "Insert LAST-READ into the current buffer.

_WRITER is a `finito-book-writer', but it's properties have no bearing on the
result of this method."
  (let* ((last-read-str (concat "Last Read: " last-read)))
    (insert last-read-str "\n")
    (overlay-put (make-overlay (1- (point)) (- (point)
                                               (length last-read-str) 1))
                 'face
                 'finito-last-read)))

(cl-defmethod finito-insert-description ((_writer finito-book-writer) description)
  "Insert DESCRIPTION into the current buffer.

_WRITER is a `finito-book-writer', but it's properties have no bearing on the
result of this method."
  (insert (concat description "\n\n"))
    (overlay-put (make-overlay (- (point) 2) (- (point) (length description) 2))
                 'face
                 'finito-book-descriptions))

(defclass finito-buffer-info ()
  ((title :initarg :title
          :type string
          :custom string
          :documentation "The title of the finito buffer.")
   (mode :initarg :mode
         :type function
         :custom function
         :documentation "The mode the finito buffer should use."))
  "A class for holding information about a finito buffer.")

(cl-defmethod finito-init-buffer ((buffer-info finito-buffer-info))
  "Initialise the current buffer according to the properties of BUFFER-INFO."
  (funcall (oref buffer-info mode)))

(defclass finito-collection-buffer-info (finito-buffer-info)
  nil
  "A class for holding information about a finito collection buffer.")

(cl-defmethod finito-init-buffer ((buffer-info finito-collection-buffer-info))
  "Initialise the current buffer according to the properties of BUFFER-INFO."
  (cl-call-next-method)
  (setq finito--collection (oref buffer-info title)))

(provide 'finito-buffer)
;;; finito-buffer.el ends here
