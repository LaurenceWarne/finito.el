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

(require 'dash)
(require 'f)
(require 'request)
(require 'url)

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
  "https://github.com/LaurenceWarne/libro-finito/releases/download/")

(defvar finito--jar-name
  (concat "finito-" finito-server-version ".jar"))

(defvar finito--server-process nil)

(defun finito--server-path ()
  "Return the path of the finito server."
  (f-join finito-server-directory finito--jar-name))

(defun finito--download-server-if-not-exists ()
  "Download a finito server if one is not already downloaded."
  (unless (f-exists-p (finito--server-path))
    (finito--download-server)))

(defun finito--download-server ()
  "Download a finito server."
  (let* ((url-base (concat finito--download-url "v" finito-server-version))
         (request-backend 'url-retrieve)
         (url (--> (concat url-base "/" finito--jar-name)
                (request it :sync t :timeout 5)
                (request-response-url it))))
    (f-mkdir finito-server-directory)
    (message "Downloading server from %s" url)
    (url-copy-file url (finito--server-path))
    (message "Finished downloading server")))

(defun finito--health-check ()
  "Return t if the finito server appears to be up, else nil."
  (ignore-errors
    (url-retrieve-synchronously (concat finito--host-uri "/health") nil nil 5)))

(defun finito--start-server-if-not-already ()
  "Start the finito server."
  (unless (f-exists-p (finito--server-path))
    (message "Server jar not found!  Please download the server"))
  (unless (or (process-live-p finito--server-process) (finito--health-check))
    (let* ((buf (generate-new-buffer "finito"))
           (command (concat "java -jar " (finito--server-path)))
           (proc (start-process-shell-command "finito" buf command)))
      (setq finito--server-process proc))))

(provide 'finito-server)
;;; finito-server.el ends here
