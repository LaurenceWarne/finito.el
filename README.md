# finito.el

`finito.el` allows for the management of books from within Emacs.  Books are presented in modified org mode buffers, and books along with user collections can be viewed/queried using [transient](https://github.com/magit/transient).

This package is a thin client for https://github.com/laurencewarne/libro-finito, more information on collection management and cutomization can be found there.  Here's a demo of some of its features:

https://user-images.githubusercontent.com/17688577/129481123-7a80b4e3-d190-4da0-a6de-0d07cf53229d.mp4

# Installation

It's not currently on melpa, but you can for example install it from source using [quelpa](https://github.com/quelpa/quelpa-use-package):

```elisp
(use-package finito
  :demand t
  :quelpa (finito :fetcher github :repo "LaurenceWarne/finito.el" :stable t)
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
   ;; command is invoked
   (lambda () (finito-start-server-if-not-already))))
```

# Keys

## Bindings Available in all finito View Buffers

| Key   | Action                                                                     |
|-------|----------------------------------------------------------------------------|
| `a`   | Add the book at point to some prompted collection                          |
| `m`   | Add the book at point to the "My Books" collection                         |
| `A`   | Search for books by the same author                                        |
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
| `C-m` | Open the "My Books" collection                                             |
| `C-r` | Open the "Currently Reading" collection                                    |

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

| Variable                               | Description                                                                       | Default                                      |
|----------------------------------------|-----------------------------------------------------------------------------------|----------------------------------------------|
| `finito-language`                      | The language search queries should request responses in                           | `"en"`                                       |
| `finito-server-directory`              | The directory the finito server should be downloaded to                           | `(concat user-emacs-directory "/finito")`    |
| `finito-img-cache-directory`           | The directory of the finito image cache                                           | `(concat finito-server-directory "/images")` |
| `finito-config-directory`              | The directory of the server config file and sqlite db                             | `"~/.config/libro-finito"`                   |
| `finito-browse-function`               | The function to be invoked by `finito-browse-book-at-point`                       | `finito--browse-function`                    |
| `finito-add-book-collection-blacklist` | Collections to ignore for `finito-add-book-at-point`                              | `("Currently Reading")`                      |
| `finito-save-last-search`              | A flag to indicate whether the arguments to the last search query should be saved | `t`                                          |

More information is available via `C-h v`.  Note if `org-display-remote-inline-images` is set, then finito will defer remote image handling to org and not create its own image cache.

# Similar Packages

## [org-books](https://github.com/lepisma/org-books)

## [calibredb.el](https://github.com/chenyanming/calibredb.el)
