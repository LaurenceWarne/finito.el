;;; finito-server.el --- Server bootstrapping for finito -*- lexical-binding: t -*-

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

;; This file contains utilities for starting and monitoring the finito
;; server.

;;; Code:

(require 'dash)
(require 'f)
(require 'request)
(require 'url)

(require 'finito-core)

(require 'async)

;;; Constants

(defconst finito-server-minimum-required-version
  "0.8.3"
  "The minimum compatible finito server version.")

;;; Custom variables

(defcustom finito-server-directory
  (f-join user-emacs-directory "finito/")
  "The directory of the finito server."
  :group 'finito
  :type 'directory)

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

(defvar finito--server-startup-timeout-msg
  "Timeout waiting for the finito server.

Are there any error messages when you (switch-to-buffer \"finito\") ?")

(defvar finito--base-uri "http://localhost:56848")

(defvar finito--host-uri nil)

(defvar finito--health-uri nil)

(defvar finito--server-path
  (f-join finito-server-directory finito--jar-name))

(defvar finito--jvm-args "-Xmx256M")

(defvar-local finito--bootstrap-complete nil)

;;; User facing functions

;;;###autoload
(defun finito-download-server-if-not-exists (&optional callback)
  "Download a finito server if one is not already downloaded.

If CALLBACK is specified, call that function when the download has
completed, or if a server has already been downloaded.

Existing server jars with versions older than `finito-server-version' will
be deleted."
  (if (f-exists-p finito--server-path)
      (when callback (funcall callback))
    (--each
        (ignore-errors (f-entries finito-server-directory))
      (when (and (s-starts-with-p "finito-" (f-filename it))
                 (f-ext-p it "jar"))
        (f-delete it)))
    (finito--download-server callback)))

(defun finito-start-server-if-not-already ()
  "Start a finito server if one does not already appear to be up.

A process obj is returned if a server process was successfully started,
and nil is returned if a server was detected to already have been started.
An error is signalled if the server jar cannot be found.

Note that the server does not necessarily need to be running as a subprocess
of the current Emacs session.  This function will return nil if any server
appears to be running at `finito--host-uri'/health"
  (unless (f-exists-p finito--server-path)
    (error finito--no-server-error-msg))
  (unless (or (process-live-p finito--server-process) (finito--health-check))
    (message "Starting a finito server...")
    (let* ((buf (generate-new-buffer "finito"))
           (command (concat "java "
                            finito--jvm-args
                            " -jar "
                            finito--server-path))
           (proc (start-process-shell-command "finito" buf command)))
      (setq finito--server-process proc))))

;;; Misc functions

(defmacro finito--wait-for-server-then (&rest body)
  "Wait for the server to start asynchronously and then run BODY."
  `(let ((buf (current-buffer)))
     (finito--wait-for-server
      (lambda () (switch-to-buffer buf) ,@body))))

(defun finito--wait-for-server (callback &optional attempts)
  "Wait for the finito server to start then call CALLBACK.

ATTEMPTS is the max number of attempts to wait (in 0.5 second increments)
until signalling an error.  The default is 20."
  (if (finito--server-subprocess-bootstrap-complete)
      (funcall callback)
    (finito-start-server-if-not-already)
    (async-start
     `(lambda ()
        ,(async-inject-variables "finito--base-uri\\|finito--health-uri")
        ;; TODO how can I inject functions with async.el?
        (defun finito--health-uri ()
          "Return the uri to use for health checks."
          (or finito--health-uri (concat finito--base-uri "/version")))
        (defun finito--health-check ()
          (not (null (ignore-errors
                       (url-retrieve-synchronously (finito--health-uri) nil nil 5)))))
        (require 'cl-lib)
        (cl-find-if
         (lambda (_) (or (finito--health-check) (ignore (sleep-for 0.5))))
         (make-list ,(or attempts 40) t)))
     (lambda (response)
       (if response
           (progn (when-let ((buf (finito--process-buffer)))
                    (with-current-buffer buf
                      (setq finito--bootstrap-complete t)))
                  (funcall callback))
         (error finito--server-startup-timeout-msg))))))

(defun finito--server-subprocess-bootstrap-complete ()
  "Return t if the the finito subprocess has completed bootstrapping else nil.

Note if finito is not running as a subprocess, this function will always
return nil."
  (when-let ((buf (finito--process-buffer)))
    (with-current-buffer buf
      (bound-and-true-p finito--bootstrap-complete))))

(defun finito--process-buffer ()
  "Return the buffer of the finito process, or nil if it does not exist."
  (when (process-live-p finito--server-process)
    (process-buffer finito--server-process)))

(defun finito--download-server (&optional callback)
  "Download a finito server asynchronously.

If CALLBACK is specified, call that function when the download has
completed.  The server version is determined by `finito-server-version',
and the the server will save the server jar to `finito--server-path'."
  (let ((request-backend 'url-retrieve))
    (make-directory finito-server-directory t)
    (message "Starting finito server download...")
    (async-start
     `(lambda ()
        ;; No regex for workaround to 'Invalid read syntax: "#"'
        ,(async-inject-variables "finito--download-url")
        ,(async-inject-variables "finito-server-version")
        ,(async-inject-variables "finito--jar-name")
        ,(async-inject-variables "finito--server-path")
        (require 'subr-x)
        (thread-first
          (concat finito--download-url "v" finito-server-version)
          (concat "/" finito--jar-name)
          (url-copy-file finito--server-path)))
     (lambda (_)
       (message "Finished downloading the finito server")
       (when callback (funcall callback))))))

(defun finito--host-uri ()
  "Return the uri to use for finito graphql calls."
  (or finito--host-uri (concat finito--base-uri "/api/graphql")))

(defun finito--health-uri ()
  "Return the uri to use for health checks."
  (or finito--health-uri (concat finito--base-uri "/version")))

(defun finito--health-check ()
  "Return t if a finito server appears to be up, else nil."
  (not (eq (ignore-errors
             (url-retrieve-synchronously finito--health-uri nil nil 5)) nil)))

(provide 'finito-server)
;;; finito-server.el ends here
