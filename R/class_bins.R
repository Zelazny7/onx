#' @include generics.R

## variable class is just a container object
# variables --> Has Performance
#  |- levels
#     |- bins
#

## Bin Abstract Class ----
#' @title Bin Abstract Class
#'
#' @description  The Bin abstract class for all unit bin levels such as
#' \link{BinNumeric} \link{BinFactor}, \link{BinMissing}, and \link{BinException}.
#'
#' @rdname Bin-class
#' @aliases Bin-class
#' @export
setClass("Bin", contains = "VIRTUAL")

#' @import methods
#' @export BinNumeric
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

#' @import methods
#' @export BinException
BinException <- setClass("BinException", slots=list(
  exception = "numeric"),
  contains = "Bin",
  validity = function(object) {
    identical(length(object@exception), 1L) && !is.na(object@exception)
  }
)

#' @import methods
#' @export BinFactor
BinFactor <- setClass("BinFactor", slots = list(
  level = "character"),
  prototype = list(
    level = ""
  ),
  contains = "Bin"
)

#' @import methods
#' @export BinMissing
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


setMethod(
  "combinable",
  signature = c("BinNumeric", "BinNumeric"),
  definition = function(a, b) {
    return(a@upper >= b@lower && b@upper >= a@lower)
  })

setMethod(
  "combinable",
  signature = c("Bin", "Bin"),
  definition = function(a, b) return(FALSE))



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

## show-Bin ----
#' @rdname show-methods
#' @aliases show,Bin-method
setMethod(
  "show",
  signature = "Bin",
  definition = function(object) {
    cat(get_label(object), sep = "\n")
  })


################# Combine/Expand ----


## combine-BinNumeric,BinNumeric ----
#' @rdname combine-methods
#' @aliases combine,BinNumeric,BinNumeric-method
setMethod(
  "combine",
  signature = c("BinNumeric", "BinNumeric"),
  definition = function(a, b) {
    if (combinable(a, b)) {
      lower <- min(a@lower, b@lower)
      upper <- max(a@upper, b@upper)
      return(BinNumeric(lower=lower, upper=upper))
    } else {
      return(list(a, b))
    }
  })


## combine-Bin,Bin ----
#' @rdname combine-methods
#' @aliases combine,Bin,Bin-method
setMethod(
  "combine",
  signature = c("Bin", "Bin"),
  definition = function(a, b) return(list(a, b)))


setMethod(
  "combine",
  signature = c("list", "Bin"),
  definition = function(a, b) {

    # try to combine b into any elements of a, otherwise append to list
    for (i in seq_along(a)) {
      if (combinable(a[[i]], b)) {
        a[[i]] <- combine(a[[i]], b)
        return(Reduce(combine, a))
      }
    }

    return(append(a, b))
  })

setMethod("combine", signature = c("Bin", "list"), function(a, b) combine(b, a))


## expand-BinNumeric,BinNumeric ----
#' @param breaks A numeric vector of two or more cut points.
#' @rdname expand-methods
#' @aliases combineexpand,BinNumeric,BinNumeric-method
setMethod(
  "expand",
  signature = c("BinNumeric"),
  definition = function(object, breaks, ...) {
    
    ## make sure breaks are between current lower and upper
    breaks <- subset(breaks, breaks <= object@upper & breaks >= object@lower)
    breaks <- sort(unique(c(object@lower, breaks, object@upper)))
    
    if (length(breaks) <= 1) return(object)
    
    l <- list()
    for (i in seq.int(length(breaks) - 1)) {
      l[[i]] <- BinNumeric(lower=breaks[[i]], upper=breaks[[i+1]])
    }
    
    return(l)
  })


## Order values ----
order_mapping <- c(
  "BinNumeric"    = 1,
  "BinFactor"     = 1,
  "BinException"  = 2,
  "BinMissing"    = 3)


setMethod("ordervalue", "character"   , function(object, ...) order_mapping[[object]])
setMethod("ordervalue", "BinNumeric"  , function(object, ...) c(ordervalue(class(object)), object@lower))

setMethod("ordervalue", "BinFactor"   , function(object, ...) {
  c(ordervalue(class(object)), as.numeric(charToRaw(object@level)))
  })

setMethod("ordervalue", "BinException", function(object, ...) c(ordervalue(class(object)), object@exception))
setMethod("ordervalue", "BinMissing"  , function(object, ...) c(ordervalue(class(object)), Inf))

