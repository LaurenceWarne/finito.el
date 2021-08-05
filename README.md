# finito.el

`finito.el` allows for the management of books from within Emacs.  Books are presented in modified org mode buffers, and books along with user collections can be viewed/queried using [transient](https://github.com/magit/transient).

This package is a thin client for https://github.com/laurencewarne/libro-finito, more information on collection management and customization can be found there.

# Installation

```elisp
(use-package finito
  :demand t
  :quelpa (finito :fetcher github :repo "LaurenceWarne/finito.el" :stable t)
  :bind ("C-c b" . finito-dispatch)
  :config
  ;; Downloads the server aysnchronously, you can also download the server
  ;; jar manually from the releases page:
  ;; https://github.com/LaurenceWarne/libro-finito/releases and place it in
  ;; `finito-server-directory'
  (finito-download-server-if-not-exists)
  ;; Optional, but will ensure Emacs is not blocked waiting for startup once
  ;; a finito command is invoked
  (finito-start-server-if-not-already))
```

# Keys

## Bindings in a Search Buffer

| Key   | Action                                                                     |
|-------|----------------------------------------------------------------------------|
| `a`   | Add the book at point to some prompted collection                          |
| `m`   | Add the book at point to the "My Books" collection                         |
| `A`   | Search for books by the same author                                        |
| `n`   | Goto the next book                                                         |
| `p`   | Goto the previous book                                                     |
| `o`   | Dump this buffers contens in a normal org mode buffer                      |
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

All of the bindings present above are present here, along with:

| Key | Action                                               |
|-----|------------------------------------------------------|
| `g` | Refresh the collection for changes                   |
| `D` | Delete the book at point from the current collection |

# Customisation

## `finito-writer-instance`

This object can be used to customize how books are written into finito buffers.  The `finito-book-writer` class can be extended to provide ad-hoc customization.  Example:

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

This variable holds the name of the collection to open when the "My Books" suffix is invoked from the `finito-dispatch` prefix command.

It can be changed to some other user created collection, though note its default value ("My Books") is marked as a [special collection](https://github.com/LaurenceWarne/libro-finito#special-collections) (by default) by the server - more specifically the **default** collection which automatically adds all books added to any other collection or started/completed/rated to itself.

Therefore, once you have:

```elisp
(setq finito-my-books-collection "good books, some say the greatest")
```

In order to accumulate all added books you will have to mark it as a special collection and add hook (or not if you prefer books not be added automagically everywhere).

## `finito-currently-reading-collection`

This variable holds the name of the collection to open when the "Currently Reading" suffix is invoked from the `finito-dispatch` prefix command.

The situation is similar to that of `finito-my-books-collection` above in that the default value "Currently Reading" is regarded as a special collection, though only books marked as "started" will be added to this collection.

## Misc Variables

| Variable                 | Description                                                 |
|--------------------------|-------------------------------------------------------------|
| `finito-language`        | The languge search queries should request responses in      |
| `finito-image-cache-dir` | The directory to cache book images                          |
| `finito-browse-function` | The function to be invoked by `finito-browse-book-at-point` |

More information is available via `C-h v`.

# Similar Packages

## [org-books](https://github.com/lepisma/org-books)

## [calibredb.el](https://github.com/chenyanming/calibredb.el)
