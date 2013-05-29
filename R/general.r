"%in%" <- function(item, thing) {
  if (class(thing)=="pylist") {
    is.na(thing$index(item))==FALSE
  } else if(class(thing)=="pydict") {
    thing$has_key(item)
  } else {
    match(item, thing, nomatch = 0) > 0
  }
}
