;;; finito-view.el --- finito server bootstrapping -*- lexical-binding: t -*-

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
;; This file contains utilities for starting and monitoring the finito
;; server.

;;; Code:

(require 'finito-core)

(defconst finito-server-minimum-required-version
  "0.1.0"
  "The minimum compatible finito server version.")

(defcustom finito-server-directory
  (f-join user-emacs-directory "finito/")
  "The directory used to cache images."
  :group 'finito
  :type 'string)

(defcustom finito-server-version
  finito-server-minimum-required-version
  "The finito server version to use."
  :group 'finito
  :type 'string)

(defvar finito--download-url
  "https://github.com/LaurenceWarne/libro-finito/releases/tag/")

(defvar finito--jar-name
  (concat "finito-" finito-server-version ".jar"))

(defun finito--download-server ()
  "Download a finito server."
  (let* ((url-base (concat finito--download-url "v" finito-server-version))
         (url (concat url-base "/" finito--jar-name)))
    (f-mkdir finito-server-directory)
    (url-copy-file url (f-join finito-server-directory finito--jar-name))))

(provide 'finito-server)
;;; finito-server.el ends here
