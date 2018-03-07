## Contains generics not specific to a single class

if (!isGeneric("predict")) setGeneric("predict", function(object, ...) standardGeneric("predict"))

setGeneric("get_exceptions", function(object, ...) standardGeneric("get_exceptions"))

## Get Boolean Mask ----
#' Get Boolean Mask
#'
#' Returns a logical vector the same length as argument \code{x} indicating
#' membership in an appropriate interval or a match to an appropriate level.
#'
#' @param object An object
#' @param x A vector of the appropriate type to map to TRUE/FALSE.
#' @param ... Additional arguments passed on to other methods.
#'
#' @export
#' @docType methods
#' @rdname get_boolean_mask-methods
setGeneric("get_boolean_mask", function(object, x, ...) standardGeneric("get_boolean_mask"))

setGeneric("len", def = function(x) standardGeneric("len"))

## Get Object Labels ----
#' Get Object Labels
#'
#' Some additional info
#'
#' @param object An object that can generate labels.
#' @param ... Additional arguments passed on to other methods.
#'
#' @export
#' @docType methods
#' @rdname get_label-methods
setGeneric("get_label", def = function(object, ...) standardGeneric("get_label"))

setGeneric("value<-", def = function(object, value) standardGeneric("value<-"))
