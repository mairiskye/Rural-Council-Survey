wrap_it <- function(x, len) {
  sapply(x,
         function(y) paste(strwrap(y, len),
                           collapse = "\n"),
         USE.NAMES = FALSE)
}

# CALL THIS ONE. Wraps long strings by a specified width.
#param: x = list of strings
#       len = (integer) width to wrap each string by
wrap_labels <- function(x, len) {
  if (is.list(x)) {
    lapply(x, wrap_it, len)
  } else {
    wrap_it(x, len)
  }
}
