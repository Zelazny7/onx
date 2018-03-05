## variable class is just a container object
# variables --> Has Performance
#  |- levels
#     |- bins
#

## S3?

## bin interface
## constuctor
## u
## predict
##

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

setGeneric("get_boolean_mask", function(object, x, ...) standardGeneric("get_boolean_mask"))

## TODO: Document that numeric bins are left-open and right-closed
setMethod(
  "get_boolean_mask",
  signature = c("BinNumeric", "numeric"),
  definition = function(object, x, ...) {
    x > object@lower & x <= object@upper & !is.na(x)
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
setGeneric("get_label", def = function(object, ...) standardGeneric("get_label"))

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