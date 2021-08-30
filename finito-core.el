;;; finito-core.el --- finito core -*- lexical-binding: t -*-

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

;; Defines the custom group used by finito, along with the version.

;;; Code:

(defconst finito-version "0.1.0")

(defgroup finito nil
  "Book and book collection management."
  :group 'applications)

(defcustom finito-language
  "en"
  "The language search queries should request for responses to be in.

 This should be a two-letter ISO-639-1 code such as \"en\" or \"fr\"."
  :group 'finito
  :type 'string)

(provide 'finito-core)
;;; finito-core.el ends here
