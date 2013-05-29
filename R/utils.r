

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

iris

?globalenv


encapsulate <- function(values) {
  unlist(lapply(values, function(x) {
    if (class(x)=="character") {
      paste("'", x, "'", sep="")
      } else {
        x
      }
  }
))
}


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
#     paste(object, collapse="\n")
  }
}

# 
# cat(dict_repl(iris, "iris"))

