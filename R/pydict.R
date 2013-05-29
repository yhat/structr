library(sets)
library(digest)

pydict <- setRefClass("pydict",
                      fields = list( data = "list", defaultvalue = "ANY", keymap = "list"),
                      methods = list(
                        show = function() {
                          'printing the list'
                          cat(string())
                        },
                        string = function() {
                          key.strings <- encapsulate(keys()$data)
                          value.strings <- encapsulate(data)
                          paste("{",
                                paste(key.strings, value.strings, sep=": ", collapse=", "),
                                "}", sep="")
                        },
                        count = function() {
                          length(data)
                        },
                        keys = function() {
                          items <- keymap
                          names(items) <- NULL
                          pylist$new(data=items)
                        },
                        iterkeys = function() {
                          seq(keys())
                        },
                        digested_keys = function() {
                          items <- names(keymap)
                          names(items) <- NULL
                          unlist(items)
                        },
                        values = function() {
                          vals <- data
                          names(vals) <- NULL
                          pylist$new(data=vals)
                        },
                        itervalues = function() {
                          seq(values())
                        },
                        count = function() {
                          'returns the number of items in the list'
                          length(data)
                        },
                        get = function(key, default=NA) {
                          if (has_key(key)) {
                            .self[key]
                          } else if(is.na(default)) {
                            defaultvalue
                          } else {
                            default
                          }
                        },
                        index = function(key) {
                          match(key, keymap)
                        },
                        pop = function(key) {
                          value <- .self$get(key)
                          if (!is.na(value)) {
                            idx <- index(key)
                            data[idx] <<- NULL
                            keymap[idx] <<- NULL
                          }
                          value
                        },
                        popitem = function() {
                          if (count() > 0) {
                            key <- keys()[1]
                            value <- unlist(.self[key])
                            idx <- index(key)
                            data[idx] <<- NULL
                            keymap[idx] <<- NULL
                            list.py(key, value)
                          } else {
                            NA
                          }
                        },
                        setdefault = function(value) {
                          defaultvalue <<- value
                        },
                        has_key = function(key) {
                          digest(key) %in% names(data) || key %in% keymap
                        },
                        update = function(adict) {
                          for (key in adict$iterkeys()) {
                            if (! has_key(key)) {
                              add_key(key, adict[key])
                            }
                          }
                        },
                        clear = function() {
                          data <<- list()
                        },
                        iteritems = function() {
                          items <- list.py()
                          for (key in iterkeys()) {
                            # TODO: should be a tuple
                            value <- unlist(.self[key])
                            names(value) <- NULL
                            item <- tuple(key, value)
                            item <- list.py(key, value)
                            items$append(item)
                          }
                          seq(items)
                        },
                        add_key = function(key, value) {
                          key.digest <- digest(key)
                          data[key.digest] <<- value
#                           obj_name <- bquote(..., globalenv())
                          keymap[key.digest] <<- dict_repl(key, "")
                        },
                        get_key = function(key) {
                          key.digest <- digest(key)
                          if (! key.digest %in% names(data)) {
                            key.digest <- names(keymap)[keymap==key]
                          }
                          item <- data[key.digest]  
                          names(item) <- NULL
                          unlist(item)
                        }
                      ))
setMethod(f="[",
          signature="pydict",
          definition=function(x, i, j, drop) {
            if (x$has_key(i)==FALSE) {
              stop(paste(i, "not in dictionary"))
            }
            x$get_key(i)
          })

setReplaceMethod(f="[",
          signature="pydict",
          definition=function(x, i, j, value) {
            x$add_key(i, value)
            return (x)
          })

setMethod("seq",
          signature="pydict",
          definition=function(adict) {
            adict$iterkeys()
          })

setMethod(f="length",
          signature="pydict",
          definition=function(x) {
            x$count()
          })


dict.py <- function(...) {
  data <- list(...)
  if (is.null(names(data)) & length(data) > 0) {
    stop("No keys specified for dictionary")
  }
  keymap <- lapply(names(data), I)
  names(keymap) <- lapply(keymap, digest)
  names(data) <- lapply(names(data), digest)
  pydict$new(data=data, keymap=keymap)
}

zip.dict <- function(x, y) {
  dict <- dict.py()
  for (i in 1:min(length(x), length(y))) {
    key <- x[i]
    dict[key] <- y[i]
  }
  dict
}

zip.tuple <- function(x, y) {
  alist <- list.py()
  for (i in 1:min(length(x), length(y))) {
    key <- x[i]
    alist$append(list.py(x[i], y[i]))
  }
  alist
}

d <- zip.tuple(1:10, 1:100 * 10)
d

d <- zip.tuple(list.py(1, 2, 3), list.py("a", "b", "c"))
d

d <- zip.tuple(1:100, 1:100 * 10)
d


d <- zip.dict(1:10, 1:100 * 10)
d

d <- zip.dict(letters, 1:100 * 10)
d


for (item in seq(zip.tuple(1:10, 1:10))) {
  print(item)
}

d <- zip.dict(1:10, 1:100 * 10)
d

# dict.py(1)
d <- dict.py(a=1, b=2)
d['a'] <- 1
d['c'] <- 3
d
d$keys()
sum(d$values())
d$get("a")
d$get("fake", 10)
d$pop("a")
d$popitem()
d
d$setdefault(100)
d$get("fakekey")

a <- dict.py(a=1, b=2)
b <- dict.py(c=1, b=0)
a$update(b)
a
b$clear()
b

a$iteritems()

test <- dict.py()
for (l in letters) {
  for (i in 1:10) {
    key <- paste(l, i, sep="")
    test[key] = i
  }
}
test['a2']
"x8" %in% test
"fakething" %in% test

x <- dict.py(a=1, b=21)
for(key in seq(x)) {
  print(key)
}

for (item in x$iteritems()) {
  print(item)
}

d <- dict.py()
d[iris] = "hello"
d
d[1] = 100
