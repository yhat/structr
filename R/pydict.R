
pydict <- setRefClass("pydict",
                      fields = list( data = "list", defaultvalue = "ANY", keymap = "list"),
                      methods = list(
                        init = function() {
                          methods <- getRefClass(class(.self))$methods()
                          eval(parse(text=paste(".self$", methods)))
                          .self
                        },
                        show = function() {
                          'visual representation of the dict'
                          cat(string())
                        },
                        string = function() {
                          'string representation of the dict'
                          key.strings <- encapsulate(keys()$data)
                          value.strings <- encapsulate(data)
                          value.strings <- lapply(value.strings, unstringify.if.df)
                          paste("{",
                                paste(key.strings, value.strings, sep=": ", collapse=", "),
                                "}", sep="")
                        },
                        count = function() {
                          'number of items in the dict'
                          length(data)
                        },
                        keys = function() {
                          'returns a pylist of the keys'
                          items <- keymap
                          names(items) <- NULL
                          pylist$new(data=items)
                        },
                        iterkeys = function() {
                          'returns an iterable of the keys'
                          seq(keys())
                        },
                        digested_keys = function() {
                          'returns a pylist of the hashed/digested keys'
                          items <- names(keymap)
                          names(items) <- NULL
                          unlist(items)
                        },
                        values = function() {
                          'returns a pylist of the dict values'
                          vals <- data
                          names(vals) <- NULL
                          pylist$new(data=vals)
                        },
                        itervalues = function() {
                          'returns an iterable of the values'
                          seq(values())
                        },
                        get = function(key, default=NA) {
                          'attempts to find the value associated with the
                          given key. if it does not exist then a default value
                          is returned (if specified).'
                          if (has_key(key)) {
                            .self[key]
                          } else if(is.na(default)) {
                            defaultvalue
                          } else {
                            default
                          }
                        },
                        index = function(key) {
                          'not sure if this should exist'
                          match(key, keymap)
                        },
                        pop = function(key) {
                          'gets the value associated with the given key and
                          removes it from the dictionary'
                          value <- .self$get(key)
                          if (!is.na(value)) {
                            idx <- index(key)
                            data[idx] <<- NULL
                            keymap[idx] <<- NULL
                          }
                          value
                        },
                        popitem = function() {
                          'gets the key value pair associated with the given key and
                          removes it from the dictionary'
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
                          'sets the default value for the dict'
                          defaultvalue <<- value
                        },
                        has_key = function(key) {
                          'determines whether or not a key is in the dict'
                          digest::digest(key) %in% names(data) || key %in% keymap
                        },
                        update = function(adict) {
                          'takes another dict and adds any keys and their 
                          corresponding values to the dict'
                          #                           for (key in adict$iterkeys()) {
                          for (key in unlist(adict$keys()$data)) {
                            if (! has_key(key)) {
                              add_key(key, adict[key])
                            }
                          }
                        },
                        clear = function() {
                          'removes all data from the dict'
                          data <<- list()
                          keymap <<- list()
                        },
                        items = function() {
                          'returns the keys and values of the dict as a
                          pylist of [key, value] pairs'
                          zip.tuple(keymap, data)
                        },
                        iteritems = function() {
                          'returns an iterable pylist of [key, value] pairs'
                          seq(items())
                        },
                        add_key = function(key, value) {
                          'private method for adding a key to the dict'
                          
                          if (class(value)=="data.frame" || (is.vector(value)==TRUE & length(value) > 1)) {
                            value <- paste("pickled: ", stringify.object(value), sep="")
                          }
                          
                          key.digest <- digest::digest(key)
                          data[key.digest] <<- value
                          #                           obj_name <- bquote(..., globalenv())
                          keymap[key.digest] <<- dict_repl(key, "")
                        },
                        get_key = function(key) {
                          'private method for retrieving a key/value from the dict'
                          key.digest <- digest::digest(key)
                          if (! key.digest %in% names(data)) {
                            key.digest <- names(keymap)[keymap==key]
                          }
                          item <- data[key.digest]
                          
                          names(item) <- NULL
                          item <- unlist(item)
                          if (is.character(item) & substring(item, 1, 9)=="pickled: ") {
                            item <- substring(item, 10)
                            item <- unstringify.object(item)
                          }
                          item
                        }
                      ))

#'Get the value of a key associated with a dictionary.
#'
#'You can use the adict['key'] syntax to access key/values from within
#'a dictionary--much like Python, Ruby, or Perl.
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

# setMethod("seq",
#           signature="pydict",
#           definition=function(adict) {
#             adict$iterkeys()
#           })

#'Function for getting the number of items in a dictionary.
#'
#'Use much like length(list()) or length(c(1, 2, 3)).
setMethod(f="length",
          signature="pydict",
          definition=function(x) {
            x$count()
          })

#'Creates an instance of a dict
#'
#'This is a wrapper function around the \code{pydict$new} that is a little
#'more R friendly.
#'
#'@param ... a series of key/value pairs in the form \code{key=value}
#'
#'@keywords dict, dict.py, key/value
#'@export
#'
#'@examples
#'(x <- dict.py("a"=1, "b"=2, "c"=3))
#'#{a: 1, b: 2, c: 3}
dict.py <- function(...) {
  data <- list(...)
  if (is.null(names(data)) & length(data) > 0) {
    stop("No keys specified for dictionary")
  }
  keymap <- lapply(names(data), I)
  names(keymap) <- lapply(keymap, digest::digest)
  names(data) <- lapply(names(data), digest::digest)
  newdict <- pydict$new(data=data, keymap=keymap)
  newdict$init()
}

#'Determines whether or not an object is an instance of a 
#'dictionary.
#'
#'Determines the class of an object and checks to see if it's a dictionary.
#'
#'@param object any object
#'@export
#'
#'@examples
#'x <- dict.py("a"=1)
#'is.dict.py(x)
#'#TRUE
#'x <- list(1, 2, 3, 4)
#'is.dict.py(x)
#'#FALSE
is.dict.py <- function(object) {
  class(object)=="pydict"
}

#'Combine 2 lists into a dict of key/values
#'
#'Takes 2 lists and converts them into a key => value mapping, which
#'takes the form of a \code{\link{dict.py}}.
#'
#'@param x a list, vector, or list.py
#'@param y a second list, vector, or list.py
#'
#'@keywords zip, dict, lists
#'
#'@export
#'@examples
#'x <- list.py(1, 2, 3)
#'y <- list.py("a", "b", "c")
#'zip.dict(x, y)
#'#{1: 'a', 2: 'b', 3: 'c'}
#'zip.dict(y, x)
#'#{'a': 1, 'b': 2, 'c': 3}
zip.dict <- function(x, y) {
  dict <- dict.py()
  for (i in 1:min(length(x), length(y))) {
    key <- x[i]
    dict[key] <- y[i]
  }
  dict
}

#'Combine 2 lists into a list of lists
#'
#'Return a list of 2 item lists, where each list contains the i-th element
#'from each of the argument sequences.  The returned list is truncated
#'in length to the length of the shortest argument sequence.
#'
#'@param x a list, vector, or list.py
#'@param y a second list, vector, or list.py
#'
#'@keywords lists, zip
#'
#'@export
#'@examples
#'x <- list.py(1, 2, 3)
#'y <- list.py(4, 5, 6)
#'zip.tuple(x, y)
#'#[[1, 4], [2, 5], [3, 6]]
#'y <- list.py("a", "b", "c")
#'zip.tuple(x, y)
#'#[[1, 'a'], [2, 'b'], [3, 'c']]
zip.tuple <- function(x, y) {
  alist <- list.py()
  for (i in 1:min(length(x), length(y))) {
    key <- x[i]
    alist$append(list.py(x[i], y[i]))
  }
  alist
}

"%in%" <- function(item, thing) {
  if (class(thing)=="pylist") {
    is.na(thing$index(item))==FALSE
  } else if(class(thing)=="pydict") {
    thing$has_key(item)
  } else {
    match(item, thing, nomatch = 0) > 0
  }
}

# d <- zip.tuple(1:10, 1:100 * 10)
# d
# 
# d <- zip.tuple(list.py(1, 2, 3), list.py("a", "b", "c"))
# d
# 
# d <- zip.tuple(1:100, 1:100 * 10)
# d
# 
# 
# d <- zip.dict(1:10, 1:100 * 10)
# d
# 
# d <- zip.dict(letters, 1:100 * 10)
# d
# 
# 
# for (item in seq(zip.tuple(1:10, 1:10))) {
#   print(item)
# }
# 
# d <- zip.dict(1:10, 1:100 * 10)
# d
# 
# # dict.py(1)
# d <- dict.py(a=1, b=2)
# d['a'] <- 1
# d['c'] <- 3
# d
# d$keys()
# sum(d$values())
# d$get("a")
# d$get("fake", 10)
# d$pop("a")
# d$popitem()
# d
# d$setdefault(100)
# d$get("fakekey")
# 
# a <- dict.py(a=1, b=2)
# b <- dict.py(c=1, b=0)
# a$update(b)
# a
# b$clear()
# b
# 
# a$iteritems()
# 
# test <- dict.py()
# for (l in letters) {
#   for (i in 1:10) {
#     key <- paste(l, i, sep="")
#     test[key] = i
#   }
# }
# test['a2']
# "x8" %in% test
# "fakething" %in% test
# 
# x <- dict.py(a=1, b=21)
# for(key in seq(x)) {
#   print(key)
# }
# 
# for (item in x$iteritems()) {
#   print(item)
# }
# 
# d <- dict.py()
# d[iris] = "hello"
# d
# d[1] = 100
