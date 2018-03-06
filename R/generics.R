## Contains generics not specific to a single class

setGeneric("get_exceptions", function(object, ...) standardGeneric("get_exceptions"))

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
