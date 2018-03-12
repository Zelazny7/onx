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

setGeneric("collapse", def = function(object, i) standardGeneric("collapse"))

## Expand an Object ----
#' Expand an Object
#'
#' Expand an expandable object such as \link{Bin-class} or \link{Level-class}
#'
#' @param object An object that can be expanded.
#' @param ... Additional arguments passed on to other methods.
#'
#' @details Expand will expand an objecet if it can, returning multiple objects
#' of the same class. Otherwise it will return the input object.
#'
#' @export
#' @docType methods
#' @rdname expand-methods
setGeneric("expand", def = function(object, ...) standardGeneric("expand"))

## Combine Objects ----
#' Combine Objects
#'
#' Combine combinable objects such as \link{Bin-class} or \link{Level-class}
#'
#' @param a First object to be combined.
#' @param b Second object to be combined.
#' @param ... Additional arguments passed on to other methods.
#'
#' @details Combine takes two objects of the same class and combines them
#' into one object of the same class.
#'
#' @export
#' @docType methods
#' @rdname combine-methods
setGeneric("combine", def = function(a, b, ...) standardGeneric("combine"))

## Test Whether Objects can be Combined ----
#' Test Whether Objects can be Combined
#'
#' Test whether two objects are combinable.
#'
#' @param a First object.
#' @param b Second object.
#'
#' @details Combinable takes two objects and returns TRUE or FALSE for whether
#' they can be combined. The \link{combine} function makes use of this test
#' to implement conditional logic.
#'
#' @export
#' @docType methods
#' @rdname combinable-methods
setGeneric("combinable", function(a, b) standardGeneric("combinable"))


#' @export
setGeneric("ordervalue", function(object, ...) standardGeneric("ordervalue"))

## Sort Objects by their OrderValue ----
#' Sort Objects by their OrderValue
#'
#' Sort a collection of objects by their \link{ordervalue}s
#'
#' @param object First object.
#' @param ... Additional arguments passed on to other methods.
#'
#' @export
#' @docType methods
#' @rdname Sort-methods
setGeneric("Sort", function(object, ...) standardGeneric("Sort"))





