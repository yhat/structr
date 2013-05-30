# source("R/utils.r")
# source("R/general.r")
# library(plyr)


pylist <- setRefClass("pylist",
                     fields = list( data = "list"),
                     methods = list(
                       show = function() {
                         'visual representation of the list
                          Examples
                          ==================================='
                         cat(string())
                       },
                       string = function() {
                         'returns a nicely formatted string representation of the list
                          Examples
                          ==================================='
                         paste("[", paste(lapply(data, function(item) {
                           if(is.character(item)) {
                             return (paste("'", item, "'", sep=""))
                             } else {
                               return (as.character(item))
                             }
                           }), collapse=", ", sep=""),
                               "]", sep="")
                       },
                       append = function(item) {
                         'adds an item to the end of the list
                          item - thing you want to add to the list
                          Examples
                          ===================================
                         '
                         data <<- base::append(data, item)
                         TRUE
                       },
                       push = function(newvalue) {
                         'adds an item to the beginning of the list
                          Examples
                          ==================================='
                         data <<- base::append(list(newvalue), data)
                         TRUE
                       },
                       pop = function() {
                         'gets the last item in the list, removes it from the list, 
                         then returns it
                         Examples
                         ==================================='
                         popval <- data[count()]
                         data <<- data[-count()]
                         unlist(popval)
                       },
                       reverse = function() {
                         'reverses the list and returns a new instance of it
                          Examples
                          ==================================='
                         revdata <- base::rev(data)
                         pylist$new(data=revdata)
                       },
                       count = function() {
                         'returns the number of items in the list
                          Examples
                          ==================================='
                         length(data)
                       },
                       insert = function(item, pos) {
                         "inserts an item at a given position in the list
                         item - what you want to insert
                         pos - index you want the item inserted into
                         Examples
                         ===================================
                         "
                         if (pos > count()) {
                           return (FALSE)
                         } else {
                           data <<- base::append(base::append(data[1:pos], item),
                                                 data[pos:count()])
                           return (TRUE)
                         }
                       },
                       index = function(item) {
                         "gets the index of an item in the list
                         item - what you're searching for in the list
                         Examples
                         ===================================
                         "
                         match(item, data)
                       },
                       sort = function() {
                         'sorts the list in place'
                         o <- base::order(unlist(data))
                         data <<- data[o]
                       },
                       map = function(fn) {
                         'applys a function to each item in the list, in place
                         fn - a function to apply to each item in the list
                         Examples
                         ===================================
                         '
                         data <<- lapply(data, fn)
                       },
                       find = function(cond) {
                         'returns items matching a condition
                          cond - can be either a function, regex, or object
                          Examples
                          ===================================
                          mylist$find(function(x) { x > 10 })
                          mylist$find("^foo")
                         '
                         if (class(cond)=="function") {
                           slice <- cond(data)
                         } else if (class(cond)=="character") {
                           slice <- grep(cond, data)  
                         }
                         pylist$new(data=data[slice])
                       },
                       contains = function(item){
                         "checks whether an item exists in the list
                          item - thing you're looking for
                          Examples
                          ===================================
                          "
                         item %in% data
                       },
                       items = function() {
                          'returns the items in the list
                          Examples
                          ===================================
                         '
                         data
                       },
                       iteritems = function() {
                         'returns an iterable of the items in the list
                          Examples
                          ===================================
                         '
                         seq(data)
                       }
                     ))

setMethod(f="[",
          signature="pylist",
          definition=function(x, i, j, drop) {
            item <- x$data[i]
            if(class(item) %in% c("list", "pylist")) {
              if (length(item) > 1) {
                return (pylist$new(data=item))  
              } else {
                return (item[[1]])
              }
            } else {
              return (unlist(item))
            }
          })

setMethod("seq",
          signature="pylist",
          definition=function(alist) {
            alist$data
          })

setMethod(f="toString",
          signature="pylist",
          definition=function(x, ...) {
            x$string()
          })

setMethod(f="as.character",
          signature="pylist",
          definition=function(x, ...) {
            x$string()
          })

setMethod(f="paste",
          signature="pylist",
          definition=function(x,  ..., sep=" ", collapse=NULL) {
            base::paste(toString(x), sep=sep, collapse=collapse)
          })

setMethod(f="hist",
          signature="pylist",
          definition=function(x) {
            hist(unlist(x$data))
          })

setMethod(f="plot",
          signature="pylist",
          definition=function(x) {
            plot(unlist(x$data))
          })

# built in numeric functions
# need to add these: http://www.statmethods.net/management/functions.html
setMethod(f="sum",
          signature="pylist",
          definition=function(x) {
            sum(unlist(x$data))
          })

setMethod(f="cumsum",
          signature="pylist",
          definition=function(x) {
            cumsum(unlist(x$data))
          })

setMethod(f="sin",
          signature="pylist",
          definition=function(x) {
            sin(unlist(x$data))
          })

setMethod(f="cos",
          signature="pylist",
          definition=function(x) {
            cos(unlist(x$data))
          })

setMethod(f="sign",
          signature="pylist",
          definition=function(x) {
            sign(unlist(x$data))
          })
# end numeric functions

setMethod(f="summary",
          signature="pylist",
          definition=function(object, ...) {
            klasses <- unlist(lapply(object$data, class))
            output <- sapply(unique(klasses), function(k) {
              mask <- klasses==k
              if (k=="character") {
                table(unlist(object$data[mask]))
              } else if(k=="pylist") {
                list("pylist"=lapply(object$data[mask], summary))
              } else {
                summary(unlist(object$data[mask]))
              }
            })
            output['count'] <- object$count()
            output
          })

setMethod(f="lapply",
          signature="pylist",
          definition=function(X, FUN, ...) {
            base::lapply(X$data, FUN, ...)
          })

setMethod(f="sapply",
          signature="pylist",
          definition=function(X, FUN, ..., simplify = TRUE,USE.NAMES = TRUE) {
            base::sapply(X$data, FUN, ..., simplify = simplify,
                         USE.NAMES = USE.NAMES)
          })

# setMethod(f="ldply",
#           signature="pylist",
#           definition=function(.data, .fun = NULL, ..., .progress = "none",
#                               .parallel = FALSE) {
#             plyr::ldply(.data$data, .fun = .fun, .progress = .progress,
#                         .parallel = .parallel, ...)
#           })
# 
# setMethod(f="llply",
#           signature="pylist",
#           definition=function(.data, .fun = NULL, ..., .progress = "none",
#                               .inform = FALSE, .parallel = FALSE) {
#             plyr::llply(.data$data, .fun = .fun, .progress = .progress,
#                         .inform = .inform, .parallel = .parallel, ...)
#             })
# 
# setMethod(f="laply",
#           signature="pylist",
#           definition=function(.data, .fun = NULL, ..., .progress = "none", 
#                               .drop = TRUE, .parallel = FALSE) {
#             plyr::laply(.data$data, .fun = .fun, .progress = .progress, 
#                         .drop = .drop, .parallel = .parallel, ...)
#           })

setMethod(f="length",
          signature="pylist",
          definition=function(x) {
            x$count()
          })

"+.pylist" <- function(x, y) {
  pylist$new(data=merge.list(x$data, y$data))
}

each <- function(alist, fn) {
  for(item in alist$data) {
    fn(item)
  }
}

#'Creates an instance of a list
#'
#'This is a wrapper function around the \code{pylist$new} that is a little
#'more R friendly.
#'
#'@param ... a series of values seperated by a comma. NOTE: a vector will be treated 
#'as an individual item. i.e. \code{list.py(1:100)} will yield a list with 1 item, whereas
#'\code{list.py(1, 2, 3, 4)} will yield a list with 4 items
#'
#'@keywords list, list.py
#'@export
#'
#'@examples
#'x <- list.py(1, 2, 3, 4)
#'#[1, 2, 3, 4]
list.py <- function(...) {
  pylist$new(data=list(...))
}

is.list.py <- function(object) {
  class(object)=="pylist"
}

"+.py" <- function(e1, e2) {
  if (is.list.py(e1) & is.list.py(e2)) {
    merge.list(x, y)
  } else if (is.dict.py(e1) & is.dict.py(e2)) {
    print()
  }
}

"%+%" <- `+.py`


# 
# d <- list.py(1, 2, 3)
# 
# x <- list.py(1, 2, 3)
# y <- list.py(4, 5, "hello")
# x + y
# 
# 2 %in% x
# 
# 
# test <- list.py(100, 200)
# test$append(300)
# test$append("hello")
# test$pop()
# test$push("hello")
# test$reverse()
# test$count()
# test$insert("hello", 100)
# test$insert("goodbye", 2)
# test$insert(TRUE, 2)
# test$index("goodbye")
# test$sort()
# test
# test <- list.py(100, 200, 300, 400)
# test$map(function(x) {
#   x * 1.5
# })
# test
# each(test, print)
# each(test, function(x) {
#   print(x * 1.5)
# })
# 
# test <- list.py("greg", "sam", "stan", "paul", "sammy")
# test$find("sam")
# test$find("^s")
# test$find("^sa")
# test <- list.py(1, 2, 3, 4, 5)
# test$find(function(x) { x > 2.5 })
# 
# test[1]
# test <- list.py(1, 2, 3, 4)
# sum(test)
# hist(test)
# plot(test)
# 
# nested <- list.py(
#   list.py(100, 200, 300),
#   list.py(400, 500)
# )
# nested
# nested[1][2]
# nested[2][1:2]
# nested[1][2]
# nested[1]
# nested
# 
# 
# x <- list.py(1, 2, 4, 5)
# x[1:2]
# nested[1][1:2]
# 
# summary(list.py(100, "austin", 200, 400, "austin", "greg"))
# summary(nested)# :(
# 
# 
# for (i in seq(x)) {
#   print(i)
# }
# 
# for (i in x$items()) {
#   print(i)
# }
# 
# lapply(d, print)
# ldply(d, function(x) { x + 1 }, .progress="text")
# 
# 
# summary(d)
# summary(nested)