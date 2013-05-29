library(sets)

pydict <- setRefClass("pydict",
                      fields = list( data = "list", defaultvalue = "ANY"),
                      methods = list(
                        show = function() {
                          'printing the list'
                          print(string())
                        },
                        string = function() {
                          paste("{",
                                paste(names(data), data, sep=": ", collapse=", "),
                                "}", sep="")
                        },
                        keys = function() {
                          names(data)
                        },
                        values = function() {
                          vals <- unlist(data)
                          names(vals) <- NULL
                          vals
                        },
                        count = function() {
                          'returns the number of items in the list'
                          length(data)
                        },
                        get = function(key, default=NA) {
                          if (key %in% names(data)) {
                            .self[key]
                          } else if(is.na(default)) {
                            defaultvalue
                          } else {
                            default
                          }
                        },
                        index = function(key) {
                          match(key, names(data))
                        },
                        pop = function(key) {
                          value <- .self$get(key)
                          if (!is.na(value)) {
                            data[index(key)] <<- NULL
                          }
                          value
                        },
                        popitem = function() {
                          if (count() > 0) {
                            key <- keys()[1]
                            value <- unlist(data[key])
                            data[index(key)] <<- NULL
                            tuple(key, value)
                          } else {
                            NA
                          }
                        },
                        setdefault = function(value) {
                          defaultvalue <<- value
                        },
                        has_key = function(key) {
                          key %in% names(data)
                        },
                        update = function(adict) {
                          for (key in adict$keys()) {
                            if (! has_key(key)) {
                              data[key] <<- adict[key]
                            }
                          }
                        },
                        clear = function() {
                          data <<- list()
                        },
                        iteritems = function() {
                          items <- list.py()
                          for (key in keys()) {
                            # TODO: should be a tuple
                            value <- unlist(data[key])
                            names(value) <- NULL
                            item <- tuple(key, value)
                            item <- list.py(key, value)
                            items$append(item)
                          }
                          seq(items)
                        }
                      ))
setMethod(f="[",
          signature="pydict",
          definition=function(x, i, j, drop) {
            if (x$has_key(i)==FALSE) {
              stop(paste(i, "not in dictionary"))
            }
            item <- x$data[i]
            names(item) <- NULL
            unlist(item)
          })

setReplaceMethod(f="[",
          signature="pydict",
          definition=function(x, i, j, value) {
            i <- paste(i)
            x$data[i] <- value
            return (x)
          })

setMethod("seq",
          signature="pydict",
          definition=function(adict) {
            adict$keys()
          })


dict.py <- function(...) {
  data <- list(...)
  if (is.null(names(data)) & length(data) > 0) {
    stop("No keys specified for dictionary")
  }
  pydict$new(data=data)
}

zip.dict <- function(x, y) {
  dict <- dict.py()
  for (i in 1:max(length(x), length(y))) {
    key <- x[i]
    dict[key] <- y[i]
  }
  dict
}

# TODO: tuples and lists not working well together 
zip.tuple <- function(x, y) {
  alist <- list()
  for (i in 1:max(length(x), length(y))) {
    key <- x[i]
    alist <- append(alist, tuple(x[i], y[i]))
  }
  alist
}

d <- zip.dict(1:100, 1:100 * 10)
d

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




