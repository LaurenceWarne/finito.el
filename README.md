# finito.el

`finito.el` allows for the management of books from within Emacs.  Books are presented in modified org mode buffers, and books along with user collections can be viewed/queried using [transient](https://github.com/magit/transient).

This package is a thin client for https://github.com/laurencewarne/libro-finito, more information on collection management and customization can be found there.

# Installation

```elisp
(use-package finito
  :quelpa (finito :fetcher github :repo "LaurenceWarne/finito.el" :stable t)
  :bind ("C-c b" . finito-dispatch))
```

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

# Similar Packages

## [org-books](https://github.com/lepisma/org-books)

## [calibredb.el](https://github.com/chenyanming/calibredb.el)
