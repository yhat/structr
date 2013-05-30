
source("./R/pylist.R")
source("./R/pydict.R")

"+.py" <- function(e1, e2) {
  if (is.list.py(e1) & is.list.py(e2)) {
    merge.list(x, y)
  } else if (is.dict.py(e1) & is.dict.py(e2)) {
    print()
  }
}


"%+%" <- `+.py`

x <- list.py(1, 2, 3)
y <- list.py(4, 5, 6)
x %+% y
x + y
merge.list(x, y)