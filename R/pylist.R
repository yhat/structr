# source("R/utils.r")
# library(plyr)


pylist <- setRefClass("pylist",
                      fields = list( data = "list"),
                      methods = list(
                        init = function() {
                          methods <- getRefClass(class(.self))$methods()
                          eval(parse(text=paste(".self$", methods)))
                          .self
                        },
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
                            } else if (class(item)=="pydict") {
                              return (item$string())
                            }else {
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
                          if (pos > count() || pos < 1) {
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

#'Get the value of an index of a list.
#'
#'You can use the adict[idx] syntax to access items from within
#'a list--much like Python, Ruby, or Perl.
#'
#' @name [
#' @aliases [,pylist-method
#' @docType methods
#' @rdname extract-methods
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

#'Set the value of an index of a list.
#'
#'You can use the alist[idx] syntax to set items within
#'a list--much like Python, Ruby, or Perl.
#'
#' @name [
#' @aliases [<-,pylist-method
#' @docType methods
#' @rdname extract-methods
setReplaceMethod(f="[",
                 signature="pylist",
                 definition=function(x, i, j, value) {
                   if (length(x) < i) {
                     stop(paste(i, "is out of bounds."))
                   }
                   x$data[i] <- value
                   return (x)
                 })

# setMethod(f="seq", "pylist", definition=function(...) {
#             print(...)
#             alist$data
#           })

#'Turns a list into a string.
#'
#'Generic function that calls the string method for a list.
setMethod(f="toString",
          signature="pylist",
          definition=function(x, ...) {
            x$string()
          })

#'Turns a list into a character vector.
#'
#'Generic function that calls the string method for a list.
#'
#'@name as.character
#'@aliases as.character,pylist-method
#'@docType methods
#'@rdname extract-methods
#'@param x a list
#'@param ... named args
#'@export
#'@aliases as.character
#'
#'@examples
#'as.character(list.py(1, 2, 3, 4))
setMethod(f="as.character",
          signature="pylist",
          definition=function(x, ...) {
            x$string()
          })

#'Turns a list into a printable string
#'
#'Generic function that calls the toString method for a list.
setMethod(f="paste",
          signature="pylist",
          definition=function(x,  ..., sep=" ", collapse=NULL) {
            base::paste(toString(x), sep=sep, collapse=collapse)
          })

#'Plots a histogram of the items of a list.
#'
#'Generic function that plots a histogram of the items in a list.
setMethod(f="hist",
          signature="pylist",
          definition=function(x) {
            hist(unlist(x$data))
          })

#'Plots a scatterplot of the items of a list.
#'
#'Generic function that plots a scatterplot of the items in a list.
setMethod(f="plot",
          signature="pylist",
          definition=function(x) {
            plot(unlist(x$data))
          })

# need to add these: http://www.statmethods.net/management/functions.html

#'Calculates the sum of the items in a list
#'
#'Generic function for caclulating the sum of the items in a list. If an item is
#'not numeric an error occurs.
#'
#'@name sum
#'@aliases sum,pylist-method
#'@docType methods
#'@rdname extract-methods
setMethod(f="sum",
          signature="pylist",
          definition=function(x) {
            sum(unlist(x$data))
          })

#'Calculates the cumsum of the items in a list
#'
#'Generic function for caclulating the cumsum of the items in a list. If an item is
#'not numeric an error occurs.
#'
#'@name cumsum
#'@aliases cumsum,pylist-method
#'@docType methods
#'@rdname extract-methods
setMethod(f="cumsum",
          signature="pylist",
          definition=function(x) {
            cumsum(unlist(x$data))
          })

#'Calculates the sin of the items in a list
#'
#'Generic function for caclulating the sin of the items in a list. If an item is
#'not numeric an error occurs.
#'
#'@name sin
#'@aliases sin,pylist-method
#'@docType methods
#'@rdname extract-methods
setMethod(f="sin",
          signature="pylist",
          definition=function(x) {
            sin(unlist(x$data))
          })

#'Calculates the cos of the items in a list
#'
#'Generic function for caclulating the cos of the items in a list. If an item is
#'not numeric an error occurs.
#'
#'@name cos
#'@aliases cos,pylist-method
#'@docType methods
#'@rdname extract-methods
setMethod(f="cos",
          signature="pylist",
          definition=function(x) {
            cos(unlist(x$data))
          })

#'Calculates the sign of the items in a list
#'
#'Generic function for caclulating the sign of the items in a list. If an item is
#'not numeric an error occurs.
#'
#'@name sign
#'@aliases sign,pylist-method
#'@docType methods
#'@rdname extract-methods
setMethod(f="sign",
          signature="pylist",
          definition=function(x) {
            sign(unlist(x$data))
          })

#'Creates a summary of the items in a list.
#'
#'Sumamrizes the list by data type. Each data type gets it's own summary with
#'the results put into a native R list.
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

#'Wrapper around \code{lapply}.
#'
#'Automatically invotes \code{lapply} on the items in the list.
setMethod(f="lapply",
          signature="pylist",
          definition=function(X, FUN, ...) {
            base::lapply(X$data, FUN, ...)
          })

#'Wrapper around \code{sapply}.
#'
#'Automatically invotes \code{sapply} on the items in the list.
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

#'Function for getting the number of items in a list
#'
#'Use much like length(list(1, 2, 3)) or length(c(1, 2, 3)).
#'
#'@name length
#'@aliases length,pylist-method
#'@docType methods
#'@rdname extract-methods
setMethod(f="length",
          signature="pylist",
          definition=function(x) {
            x$count()
          })

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
  newlist <- pylist$new(data=list(...))
  newlist$init()
}

#'Determines whether or not an object is an instance of a 
#'list
#'
#'Determines the class of an object and checks to see if it's a list
#'
#'@param object any object
#'@export
#'
#'@examples
#'x <- list.py("a")
#'is.list.py(x)
#'#TRUE
#'x <- 1:10
#'is.dict.py(x)
#'#FALSE
is.list.py <- function(object) {
  class(object)=="pylist"
}


"+.py" <- function(e1, e2) {
  if (is.list.py(e1) & is.list.py(e2)) {
    merge.list(e1, e2)
  } else if (is.dict.py(e1) & is.dict.py(e2)) {
    print()
  }
}

"%+%" <- `+.py`

"+.pylist" <- function(x, y) {
  pylist$new(data=merge.list(x$data, y$data))
}

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
