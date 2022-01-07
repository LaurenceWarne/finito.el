# finito.el

[![MELPA](https://melpa.org/packages/finito-badge.svg)](https://melpa.org/#/finito) [![Min Emacs Version](https://img.shields.io/badge/Emacs-27+-7F5AB6?logo=gnu-emacs)](https://www.gnu.org/software/emacs/) [![Version](https://img.shields.io/github/v/tag/laurencewarne/finito.el?label=release)](CHANGELOG.md)

`finito.el` allows for the management of books and reading lists from within Emacs.  Books are presented in modified org mode buffers, and books along with user collections can be viewed/queried using [transient](https://github.com/magit/transient).

This package is a thin client for https://github.com/laurencewarne/libro-finito, more information on collection management and cutomization can be found there.  Here's a demo of some of its features:

https://user-images.githubusercontent.com/17688577/129899845-c9b67d25-bf99-41f8-bbd6-04b0f47cdc97.mp4

https://user-images.githubusercontent.com/17688577/132958359-563e9940-9105-4c36-802c-cf34dc96e8c0.mp4

# Table of Contents
<!--ts-->
   * [Installation](#installation)
   * [Keys](#keys)
      * [Bindings Available in all finito View Buffers](#bindings-available-in-all-finito-view-buffers)
      * [Bindings in a Collection Buffer](#bindings-in-a-collection-buffer)
      * [Bindings in a Search Buffer](#bindings-in-a-search-buffer)
   * [Additional Commands](#additional-commands)
   * [Cutomization](#cutomization)
      * [finito-writer-instance](#finito-writer-instance)
      * [finito-my-books-collection](#finito-my-books-collection)
      * [finito-currently-reading-collection](#finito-currently-reading-collection)
      * [Misc Variables](#misc-variables)
   * [Changelog](#changelog)
   * [Similar Packages](#similar-packages)
      * [<a href="https://github.com/lepisma/org-books">org-books</a>](#org-books)
      * [<a href="https://github.com/chenyanming/calibredb.el">calibredb.el</a>](#calibredbel)

<!-- Added by: laurencewarne, at: Sat  4 Sep 11:08:17 BST 2021 -->

<!--te-->

# Installation

You can install it from melpa:

```elisp
(use-package finito
  :demand t
  ;; The main entry point for finito commands
  :bind ("C-c b" . finito)
  :config
  ;; Downloads the server aysnchronously, you can also download the server
  ;; jar manually from the releases page:
  ;; https://github.com/LaurenceWarne/libro-finito/releases and place it in
  ;; `finito-server-directory'
  (finito-download-server-if-not-exists
   ;; Optional, but we can specify a callback to run when the server has
   ;; finished downloading, we choose here to start the server to override
   ;; the default lazy behaviour which starts the server whenever a finito
   ;; command is invoked.
   ;; Note this callback is still called in the event a server has already
   ;; been downloaded
   (lambda () (finito-start-server-if-not-already))))
```

# Keys

## Bindings Available in all finito View Buffers

| Key   | Action                                                                     |
|-------|----------------------------------------------------------------------------|
| `a`   | Add the book at point to some prompted collection                          |
| `m`   | Add the book at point to the "My Books" collection                         |
| `A`   | Search for books by the same author as the book at point                   |
| `n`   | Goto the next book                                                         |
| `p`   | Goto the previous book                                                     |
| `o`   | Dump this buffer's contents in a normal org mode buffer                    |
| `q`   | Kill the current buffer                                                    |
| `k`   | Kill the current buffer                                                    |
| `b`   | Open an [Open Library](https://openlibrary.org) page for the book at point |
| `r`   | Rate the book at point                                                     |
| `s`   | Start the book at point                                                    |
| `S`   | Start the book at point at a prompted date                                 |
| `f`   | Finish the book at point                                                   |
| `F`   | Finish the book at point at a prompted date                                |
| `e`   | Search for books in the same series*                                        |
| `w`   | Copy the title of the book at point to the kill ring                       |
| `C-m` | Open the "My Books" collection                                             |
| `C-r` | Open the "Currently Reading" collection                                    |

*Results may vary, thoughts on the Wikidata [implementation](https://github.com/LaurenceWarne/libro-finito/blob/1ae5766f880b4899f0d77f2a8cdf4238e2a15c3c/finito/core/src/fin/service/book/WikidataSeriesInfoService.scala#L72) are very welcome.  If you see 'Could not find a series for xyz', you may find that it's just the case that the relevant Wikidata information is incomplete, in which case Wikidata welcomes [contributions](https://www.wikidata.org/wiki/Wikidata:Contribute)!

## Bindings in a Collection Buffer

| Key | Action                                               |
|-----|------------------------------------------------------|
| `g` | Refresh the collection for changes                   |
| `D` | Delete the book at point from the current collection |

(in addition to all the [base bindings](#bindings-available-in-all-finito-view-buffers))

## Bindings in a Search Buffer

| Key | Action                                               |
|-----|------------------------------------------------------|
| `l` | Replay the last search                               |

(in addition to all the [base bindings](#bindings-available-in-all-finito-view-buffers))

# Additional Commands

- `finito-create-book` - will prompt you to create a book with custom attributes, useful if you cannot find the book you want through searching
- `finito-delete-data-for-book-at-point` - deletes all held data about the book at point (except for its collection membership), use this for example if you set the incorrect date started/ended
- `finito-open-playground` - opens the finito server's graphql playground

# Cutomization

## `finito-writer-instance`

This object can be used to customize how books are written into finito buffers.  The `finito-book-writer` class can be extended to provide ad-hoc cutomization.  Example:

```elisp
(defclass my-book-writer (finito-book-writer)
  nil
  "My class for writing book information to a buffer.")

(cl-defmethod finito-insert-title ((_writer my-book-writer) title)
  (insert (concat "* " title "\n\n")))

(setq finito-writer-instance (my-book-writer))
```

This writer class will insert titles as level one headings, and otherwise behave exactly the same way as the default writer.

## `finito-my-books-collection`

This variable holds the name of the collection to open when the "My Books" suffix is invoked from the `finito` prefix command.

It can be changed to some other user created collection, though note its default value ("My Books") is marked as a [special collection](https://github.com/LaurenceWarne/libro-finito#special-collections) (by default) by the server - more specifically the **default** collection which automatically adds all books added to any other collection or started/completed/rated to itself.

Therefore, once you have:

```elisp
(setq finito-my-books-collection "good books, some say the greatest")
```

In order to accumulate all added books you will have to mark it as a special collection and add hooks (or not if you prefer books not be added automagically everywhere).

## `finito-currently-reading-collection`

This variable holds the name of the collection to open when the "Currently Reading" suffix is invoked from the `finito` prefix command.

The situation is similar to that of `finito-my-books-collection` above in that the default value "Currently Reading" is regarded as a special collection, though only books marked as "started" will be added to this collection.

## Misc Variables

| Variable                               | Description                                                                             | Default                                      |
|----------------------------------------|-----------------------------------------------------------------------------------------|----------------------------------------------|
| `finito-language`                      | The language search queries should request responses in                                 | `"en"`                                       |
| `finito-server-directory`              | The directory the finito server should be downloaded to                                 | `(concat user-emacs-directory "/finito")`    |
| `finito-img-cache-directory`           | The directory of the finito image cache                                                 | `(concat finito-server-directory "/images")` |
| `finito-config-directory`              | The directory of the server config file and sqlite db                                   | `"~/.config/libro-finito"`                   |
| `finito-browse-function`               | The function to be invoked by `finito-browse-book-at-point`                             | `finito--browse-function`                    |
| `finito-add-book-collection-blacklist` | Collections to ignore for `finito-add-book-at-point`                                    | `("Currently Reading")`                      |
| `finito-save-last-search`              | A flag to indicate whether the arguments to the last search query should be saved       | `t`                                          |
| `finito-use-image-uris`                | A flag to indicate whether to insert image uris or image file names into finito buffers | `nil`                                        |

More information is available via `C-h v`.  A non-nil value for `finito-use-image-uris` can be useful if for example you are using `org-display-remote-inline-images`.

# Changelog

The changelog can be viewed [here](CHANGELOG.md).

# Similar Packages

## [org-books](https://github.com/lepisma/org-books)

## [calibredb.el](https://github.com/chenyanming/calibredb.el)
