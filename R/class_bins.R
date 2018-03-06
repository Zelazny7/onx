#' @include generics.R

## variable class is just a container object
# variables --> Has Performance
#  |- levels
#     |- bins
#

### TODO: Start documenting this stuff NOW

setClass("Bin", contains = "VIRTUAL")

BinNumeric <- setClass("BinNumeric", slots = list(
    lower = "numeric",
    upper = "numeric"),
  prototype = list(
    lower = -Inf,
    upper = Inf),
  validity = function(object) {
    valid <- TRUE
    if (object@upper <= object@lower) {
      valid <- FALSE
    }
    return(valid)
  },
  contains = "Bin"
)

BinException <- setClass("BinException", slots=list(
  exception = "numeric"),
  contains = "Bin",
  validity = function(object) {
    identical(length(object@exception), 1L) && !is.na(object@exception)
  }
)

BinFactor <- setClass("BinFactor", slots = list(
  level = "character"),
  prototype = list(
    level = ""
  ),
  contains = "Bin"
)

BinMissing <- setClass("BinMissing", contains = "Bin")

setMethod(
  "get_exceptions",
  signature = c("ANY"),
  definition = function(object, ...) return(NULL)
)

setMethod(
  "get_exceptions",
  signature = c("BinException"),
  definition = function(object, ...) return(object@exception)
)

## TODO: Document that numeric bins are left-open and right-closed
setMethod(
  "get_boolean_mask",
  signature = c("BinNumeric", "numeric"),
  definition = function(object, x, ..., mask=FALSE) {
    x > object@lower & x <= object@upper & !is.na(x) & !mask
  }
)

setMethod(
  "get_boolean_mask",
  signature = c("BinFactor", "factor"),
  definition = function(object, x, ...) {
    x == object@level & !is.na(x)
  }
)

setMethod(
  "get_boolean_mask",
  signature = c("BinException", "numeric"),
  definition = function(object, x, ...) {
    x == object@exception & !is.na(x)
  }
)

setMethod(
  "get_boolean_mask",
  signature = c("BinMissing", "ANY"),
  definition = function(object, x, ...) is.na(x)
)


setGeneric("combine_bins", function(a, b) standardGeneric("combine_bins"))
setGeneric("is_valid_combination", function(a, b) standardGeneric("is_valid_combination"))

setMethod(
  "is_valid_combination",
  signature = c("BinNumeric", "BinNumeric"),
  definition = function(a, b) {
    return(a@upper >= b@lower && b@upper >= a@lower)
  })

### Used for reduction?
setMethod(
  "combine_bins",
  signature = c("BinNumeric", "BinNumeric"),
  definition = function(a, b) {
    if (is_valid_combination(a, b)) {
      lower <- min(a@lower, b@lower)
      upper <- max(a@upper, b@upper)
      return(BinNumeric(lower=lower, upper=upper))
    } else {
      return(list(a, b))
    }
  })


## get_label ----
#' @param digits rounding digits for numeric bin boundaries
#' @rdname get_label-methods
#' @aliases get_label,BinNumeric-method
setMethod(
  "get_label",
  signature = "BinNumeric",
  definition = function(object, digits=3, ...) {
    l <- round(object@lower, digits)
    u <- round(object@upper, digits)
    paste0("(", l, " - ", u, "]")
  })

setMethod(
  "get_label",
  signature = "BinFactor",
  definition = function(object, ...) {
    object@level
  })

setMethod(
  "get_label",
  signature = "BinMissing",
  definition = function(object, ...) {
    "Missing"
  })

setMethod(
  "get_label",
  signature = "BinException",
  definition = function(object, ...) {
    object@exception
  })