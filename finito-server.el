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

(eval-when-compile (require 'async))

;;; Constants

(defconst finito-server-minimum-required-version
  "0.2.0"
  "The minimum compatible finito server version.")

;;; Custom variables

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

;;; Internal variables

(defvar finito--download-url
  "https://github.com/LaurenceWarne/libro-finito/releases/download/")

(defvar finito--jar-name
  (concat "finito-" finito-server-version ".jar"))

(defvar finito--server-process nil)

(defvar finito--no-server-error-msg
  "Server jar not found!  Please download the server via:

1. Calling `finito-download-server-if-not-exists'
2. Visiting https://github.com/LaurenceWarne/libro-finito/releases and placing
   the \"finito-<version>.jar\" in `finito-server-directory'.")

(defvar finito--download-server-timeout-msg
  "Timeout downloading the finito server, are you connected to the internet?")

(defvar finito--host-uri "http://localhost:8080/api/graphql")

(defvar finito--server-path
  (f-join finito-server-directory finito--jar-name))

;;; User facing functions

(defun finito-download-server-if-not-exists ()
  "Download a finito server if one is not already downloaded."
  (unless (f-exists-p finito--server-path)
    (finito--download-server)))

;;; Misc functions

(defun finito--download-server ()
  "Download a finito server asynchronously.

The server version is determined by `finito-server-version', and the the
server will save it to the file `finito--server-path'."
  (let* ((request-backend 'url-retrieve))
    (make-directory finito-server-directory t)
    (message "Starting finito server download...")
    (async-start
     `(lambda ()
        ,(async-inject-variables "^finito-")
        (require 'subr-x)
        (thread-first
            (concat finito--download-url "v" finito-server-version)
          (concat "/" finito--jar-name)
          (url-copy-file finito--server-path)))
     (lambda (result) (message "Finished downloading the finito server")))))

(defun finito--health-check ()
  "Return t if the finito server appears to be up, else nil."
  (ignore-errors
    (url-retrieve-synchronously (concat finito--host-uri "/health") nil nil 5)))

(defun finito--start-server-if-not-already ()
  "Start the finito server."
  (unless (f-exists-p finito--server-path)
    (message finito--no-server-error-msg))
  (unless (or (process-live-p finito--server-process) (finito--health-check))
    (let* ((buf (generate-new-buffer "finito"))
           (command (concat "java -jar " finito--server-path))
           (proc (start-process-shell-command "finito" buf command)))
      (setq finito--server-process proc))))

(provide 'finito-server)
;;; finito-server.el ends here
