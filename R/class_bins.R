#' @include generics.R

## variable class is just a container object
# variables --> Has Performance
#  |- levels
#     |- bins
#

## Bin Abstract Class ----
#' Bin Abstract Class
#'
#' The Bin abstract class for all unit bin levels such as \link{BinNumeric}
#' \link{BinFactor}, \link{BinMissing}, and \link{BinException}.
#'
#' @rdname Bin-class
#' @aliases Bin-class
#' @export
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

## get_boolean_mask-BinNumeric ----
#' @rdname get_boolean_mask-methods
#' @aliases get_boolean_mask,BinNumeric,factor-method
setMethod(
  "get_boolean_mask",
  signature = c("BinNumeric", "numeric"),
  definition = function(object, x, ..., mask=FALSE) {
    x > object@lower & x <= object@upper & !is.na(x) & !mask
  }
)

## get_boolean_mask-BinFactor ----
#' @rdname get_boolean_mask-methods
#' @aliases get_boolean_mask,BinFactor,factor-method
setMethod(
  "get_boolean_mask",
  signature = c("BinFactor", "factor"),
  definition = function(object, x, ...) {
    x == object@level & !is.na(x)
  }
)

## get_boolean_mask-BinException ----
#' @rdname get_boolean_mask-methods
#' @aliases get_boolean_mask,BinException,numeric-method
setMethod(
  "get_boolean_mask",
  signature = c("BinException", "numeric"),
  definition = function(object, x, ...) {
    x == object@exception & !is.na(x)
  }
)

## get_boolean_mask-BinMissing ----
#' @rdname get_boolean_mask-methods
#' @aliases get_boolean_mask,BinMissing,ANY-method
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


## get_label-BinNumeric ----
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

## get_label-BinFactor ----
#' @rdname get_label-methods
#' @aliases get_label,BinFactor-method
setMethod(
  "get_label",
  signature = "BinFactor",
  definition = function(object, ...) {
    object@level
  })

## get_label-BinMissing ----
#' @rdname get_label-methods
#' @aliases get_label,BinMissing-method
setMethod(
  "get_label",
  signature = "BinMissing",
  definition = function(object, ...) {
    "Missing"
  })

## get_label-BinException ----
#' @rdname get_label-methods
#' @aliases get_label,BinException-method
setMethod(
  "get_label",
  signature = "BinException",
  definition = function(object, ...) {
    object@exception
  })
