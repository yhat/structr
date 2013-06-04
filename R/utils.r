
#'Function that takes 2 lists and merges them fairly effeciently
#'
#'@param x a list
#'@param y a second list
#'@param mergeUnnamed boolean for whether or not to include list items with no names
#'@param ... whatever else you've got
merge.list <- function(x, y=NULL, mergeUnnamed=TRUE, ...) {
  if (is.null(y)) {
    return(as.list(x))
  } else {
    y <- as.list(y)
    
    # replace NA in names with ""
    if (!is.null(names(x))) {
      names(x)[is.na(names(x))] <- ""
    }
    if (!is.null(names(y))) {
      names(y)[is.na(names(y))] <- ""
    }
    
    names1 <- names(x)
    names2 <- names(y)
    
    # select an element if it has:
    # a.) an empty name in y _and_
    #     mergeUnnamed is true _or_
    # b.) a name _not_ contained in x
    isUnique <- if (mergeUnnamed) {
      if (is.null(names2) | is.null(names1)) {
        rep(TRUE, length(y))
      } else {
        (nchar(names2) == 0) | !(names2 %in% names1)
      }
    } else {
      !(names2 %in% names1) & (nchar(names2) != 0)
    }
    mergedList <- c(x, y[isUnique])
    return(mergedList)
  }
}

#'Helper function for making character vectors have quotes around each item when
#'printed to the console.
#'
#'@param values a vector of values
encapsulate <- function(values) {
  lapply(values, function(x) {
    if (is.character(x)) {
      paste("'", x, "'", sep="")
    } else {
      x
    }
  })
}

#'Function for representing hashed objects as strings
#'
#'Purely visual.
#'
#'@param object an arbitrary thing
#'@param obj_name name of the variable as defined by the user (not currently being used)
dict_repl <- function(object, obj_name) {
  if (class(object)=="data.frame") {
    paste('"',
        paste(
          paste("data.frame(",sep=""),
          paste("", colnames(object), "=", head(object), "...", sep="", collapse=",\n"),
          sep=""
        ),
          ')"', sep="")
  } else {
    object
  }
}


