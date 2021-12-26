;;; finito-buffer.el --- Buffer utilities for finito -*- lexical-binding: t -*-

;; Copyright (C) 2021 Laurence Warne

;; Author: Laurence Warne

;; Local variables:
;; package-lint-main-file: "finito.el"
;; end:

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

;; This file contains utilities for working with libro finito buffers

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 'iso8601)
(require 'org)
(require 's)
(require 'savehist)

(require 'finito-core)

;;; Faces

(defface finito-author-name
  '((t :foreground "aquamarine"
       :weight bold
       :underline t))
  "Face for author names."
  :group 'finito)

(defface finito-book-descriptions
  '((t :slant oblique
       :inherit default))
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

(defface finito-summary-read
  '((t :foreground "purple"
       :weight bold))
  "Face for the number of read books in a finito summary buffer."
  :group 'finito)

(defface finito-summary-added
  '((t :foreground "sky blue"
       :weight bold))
  "Face for the number of added books in a finito summary buffer."
  :group 'finito)

(defface finito-summary-average-rating
  '((t :foreground "gold"))
  "Face for the average rating in a summary buffer."
  :group 'finito)

;;; Custom variables

(defcustom finito-use-image-uris
  nil
  "If non-nil, insert raw image uris instead of downloading image thumbnails.

This option will disable the image cache.  It's useful if for example you are
using `org-display-remote-inline-images'.  In this case org would handle
display of images even if they are remote uris.

See also URL 'https://github.com/LaurenceWarne/finito.el/issues/2' for more
information on using `org-display-remote-inline-images' with finito."
  :group 'finito
  :type 'boolean)

(defcustom finito-summary-show-recommended
  t
  "If non-nil, output a list of recommended sources for book searching."
  :group 'finito
  :type 'boolean)

(defcustom finito-show-descriptions-default
  t
  "If non-nil, write descriptions when displaying books in a finito buffer.

This can be overridden locally on a buffer by buffer basis via the
\"d\" key, or alternatively via the `finito--show-descriptions' local variable."
  :group 'finito
  :type 'boolean)

;;; Constants

(defconst finito--summary-recommended-text
  "** Looking for books?

- If you're interested in science fiction or fantasy check out the [[https://en.wikipedia.org/wiki/Hugo_Award_for_Best_Novel][Hugo Awards]] and the [[https://en.wikipedia.org/wiki/Nebula_Award_for_Best_Novel][Nebula Awards]]
- For general fiction there is the [[https://en.wikipedia.org/wiki/Pulitzer_Prize_for_Fiction][Pulitzer Prize]]
- You can ask for recommendations on [[https://www.reddit.com/r/books/][r/books]]
- Also check out [[https://openlibrary.org/][openlibrary.org]]")

;;; Internal variables

(defvar finito-show-description-alist nil)
(cl-pushnew 'finito-show-description-alist savehist-additional-variables)

;;; Buffer local variables

(defvar-local finito--ewoc
  nil
  "The ewoc used to generate the text in finito buffers.

Its nodes should be alists of the form returned by
`finito--create-book-alist'.")

(defvar-local finito--collection
  nil
  "The name of the current collection.")

;;; Classes for working with buffers

(defclass finito-book-writer ()
  nil
  "A class for writing book information to a buffer.")

(cl-defmethod finito-insert-book ((writer finito-book-writer) book-alist)
  "Write BOOK-ALIST into the current buffer using WRITER.

BOOK-ALIST is an alist of the format returned by `finito--create-book-alist'"
  (let-alist book-alist
    (finito-insert-title writer .title)
    (finito-insert-image
     writer
     (if finito-use-image-uris
         .img-uri
       .image-file-name))
    (finito-insert-author writer .authors)
    (when .rating (finito-insert-rating writer .rating))
    (when .started-reading
      (finito-insert-started-reading writer .started-reading))
    (when .last-read
      (finito-insert-last-read writer .last-read))
    (finito-insert-description writer .description)))

(cl-defmethod finito-insert-title ((_ finito-book-writer) title)
  "Insert TITLE into the current buffer."
  (insert "** " title "\n\n"))

(cl-defmethod finito-insert-image ((_ finito-book-writer) image)
  "Insert IMAGE (an image file name) into the current buffer."
  (insert "[[" image "]]  "))

(cl-defmethod finito-insert-author ((_ finito-book-writer) authors)
  "Insert AUTHORS into the current buffer."
  (let ((authors-str (s-join ", " authors)))
    (insert authors-str "\n\n")
    (overlay-put (make-overlay (- (point) 2) (- (point) (length authors-str) 2))
                 'face
                 'finito-author-name)))

(cl-defmethod finito-insert-rating ((_ finito-book-writer) rating)
  "Insert RATING into the current buffer."
  (insert (make-string (min rating 100) ?★) "\n")
  (overlay-put (make-overlay (1- (point)) (- (point) (min rating 100) 1))
                 'face
                 'finito-rating))

(cl-defmethod finito-insert-started-reading
  ((_ finito-book-writer) _started-reading)
  "Insert STARTED-READING into the current buffer."
  (let ((currently-reading-str "⌛ In Progress ⌛"))
    (insert currently-reading-str "\n")
    (overlay-put (make-overlay (1- (point)) (- (point)
                                               (length currently-reading-str) 1))
                 'face
                 'finito-currently-reading)))

(cl-defmethod finito-insert-last-read
  ((_ finito-book-writer) last-read)
  "Insert LAST-READ into the current buffer."
  (let* ((last-read-str (concat "Last Read: " last-read)))
    (insert last-read-str "\n")
    (overlay-put (make-overlay (1- (point)) (- (point)
                                               (length last-read-str) 1))
                 'face
                 'finito-last-read)))

(cl-defmethod finito-insert-description ((_ finito-book-writer) description)
  "Insert DESCRIPTION into the current buffer."
  (when (or (bound-and-true-p finito--show-descriptions)
            (not (bound-and-true-p finito--collection))
            (alist-get finito--collection
                       finito-show-description-alist
                       finito-show-descriptions-default
                       nil
                       'equal))
    (insert description "\n")
    (overlay-put (make-overlay (- (point) 2) (- (point) (length description) 2))
                 'face
                 'finito-book-descriptions)))

(defclass finito-buffer-info ()
  ((title :initarg :title
          :type string
          :custom string
          :documentation "The title of the finito buffer.")
   (buf-name :initarg :buf-name
             :type string
             :custom string
             :documentation "The name of the finito buffer.")
   (buf-name-unique :initarg :buf-name-unique
                    :type boolean
                    :custom boolean
                    :documentation
                    "If non-nil delete buffers of the same name.")
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

(defun finito--init-summary-buffer (summary-alist)
  "Create a summary buffer using data from SUMMARY-ALIST."
  (switch-to-buffer (generate-new-buffer "finito summary"))
  (finito-summary-mode)
  (let ((inhibit-read-only t))
    (let-alist summary-alist
      (insert "* Year In Books\n\n")
      (insert (format "[[%s]]\n\n" .montage-path))
      (insert "** Your Statistics\n\n")
      (insert (format "- You've read %d" .read))
      (overlay-put
       (make-overlay (- (point) (length (number-to-string .read))) (point))
       'face 'finito-summary-read)
      (insert (format " books in %d and added a total of %d." 2021 .added))
      (overlay-put
       (make-overlay (1- (- (point) (length (number-to-string .added))))
                     (1- (point)))
       'face 'finito-summary-added)
      (let ((rating-str (if (= (floor .average-rating) .average-rating)
                            (number-to-string (floor .average-rating))
                          (format "%0.2f" .average-rating))))
        (insert (format "\n- You gave an average rating of %s." rating-str))
        (overlay-put
         (make-overlay (- (point) (length (number-to-string .read)) 1)
                       (- (point) 1))
         'face 'finito-summary-average-rating))
      (when finito-summary-show-recommended
        (insert (concat "\n\n" finito--summary-recommended-text)))
      (org-display-inline-images)
      (goto-char (point-min)))))

(provide 'finito-buffer)
;;; finito-buffer.el ends here
