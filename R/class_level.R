#' @include class_bins.R generics.R

setClassUnion("LevelContinuousAppendable", c("BinNumeric", "BinMissing", "BinException"))
setClassUnion("LevelDiscreteAppendable", c("BinFactor", "BinMissing"))


## A level maintains a list of bins
## has operations for combining them
## a value assignment mapping as well
## A label
## add bins
## remove bins

## Level Class ----
#' @title Level Class
#'
#'@slot bins A list of bins contained in this level.
#'@slot values A named list of values associated with the level.
#'
#' @description  The level class consists of two main types: \code{LevelContinuous} and
#' \code{LeveLDiscrete}. Both differ in their operations and what bins subtypes they can
#' manage..
#'
#' @rdname Level-class
#' @aliases Level-class
#' @export
setClass("Level", slots = c(bins="list", values="list"), contains="VIRTUAL")

setMethod(
  "initialize",
  "Level",
  function(.Object, bins=list(BinMissing()), values=list(value=NaN)) {

    if (length(bins) > 1) bins <- Reduce(combine, bins)
    .Object@bins <- bins[!duplicated(bins)]
    .Object@values <- values
    validObject(.Object)
    .Object
  }
)

#' @description \code{LevelContinuous} object contain a list of bins each of which
#' belongs to the LevelContinuousAppendable class union. These bin types consist of
#' \code{BinNumeric}, \code{BinException}, and \code{BinMissing.}
#'
#' @rdname Level-class
#' @aliases LevelContinuous-class
#' @export
LevelContinuous <- setClass("LevelContinuous", contains="Level")

setValidity(
  "LevelContinuous",
  function(object) {
    if (identical(object@bins, list())) {
      res <- TRUE
    } else {
      res <- all(vapply(object@bins, is, TRUE, "LevelContinuousAppendable"))
    }
    if (res) res else "\"bins\" must be of type {\"BinNumeric\", \"BinException\", \"BinMissing\"}"
  })

LevelDiscrete <- setClass("LevelDiscrete", contains="Level")

setValidity(
  "LevelDiscrete",
  function(object) {
    res <- all(vapply(object@bins, is, TRUE, "LevelDiscreteAppendable"))
    if (res) res else "\"bins\" must be of type {\"BinFactor\", \"BinMissing\"}"
  })


setMethod("len", "Level", function(x) length(x@bins))

setMethod(
  "get_boolean_mask",
  signature = c("Level", "ANY"),
  definition = function(object, x, ...) {
    Reduce(`|`, lapply(object@bins, get_boolean_mask, x, ...))
  })

setMethod(
  "value<-",
  signature = c("Level", "list"),
  definition = function(object, value) {
    object@values <- modifyList(object@values, value)
    object
  }
)

## generate the labels on-demand
setMethod(
  "get_label",
  signature = c("Level"),
  definition = function(object, ...) {
    do.call(paste, c(lapply(object@bins, get_label, ...), sep=", "))
  })

setGeneric("add_bin", def = function(object, bin) standardGeneric("add_bin"))

setMethod(
  "add_bin",
  signature = c("Level", "Bin"),
  definition = function(object, bin) {
    object@bins <- append(object@bins, bin)
    Sort(object)
  })

setMethod(
  "get_exceptions",
  signature = c("Level"),
  definition = function(object, ...) {
    do.call(c, lapply(object@bins, get_exceptions))
  })

setMethod(
  "show",
  signature = "Level",
  definition = function(object) {
    cat(sprintf("%20s => %s", get_label(object), object@values[["value"]]))
  })


setMethod(
  "Sort",
  signature = "Level",
  definition = function(object, ...) {
    bins <- Reduce(combine, c(object@bins))
    new(class(object), bins=bins)
  }
)

################# Combine/Expand ----


## combine-Level,Level ----
#' @rdname combine-methods
#' @aliases combine,Level,Level-method
setMethod(
  "combine",
  signature = c("Level", "Level"),
  definition = function(a, b) {

    bins <- Reduce(combine, c(a@bins, b@bins))

    ## sort by type then value within type
    v <- do.call(rbind, Map(ordervalue, bins))
    i <- order(v[,1], v[,2])

    out <- new(class(a), bins=bins[i])

    tryCatch(
      validObject(out, complete = TRUE),
      finally = return(out))

  })

## combine-Level,Level ----
#' @rdname combine-methods
#' @aliases combine,List,Level-method
setMethod(
  "combine",
  signature = c("list", "Level"),
  definition = function(a, b) {

    stopifnot(all(sapply(a, is, "Level")))

    out <- Reduce(combine, c(a, list(b)))

    tryCatch(
      validObject(out, complete = TRUE),
      finally = return(out))

  })

## combine-Level,Level ----
#' @rdname combine-methods
#' @aliases combine,List,Level-method
setMethod("combine", c("Level", "list"), function(a, b) combine(b, a))


setMethod("combine", c("list", "missing"), function(a, b) {
  combine(head(a, -1), tail(a, 1)[[1]])
})


setMethod("ordervalue", "Level", function(object, ...) {
  v <- do.call(rbind, Map(ordervalue, object@bins))
  i <- order(v[,1], v[,2])
  v[i==1,,drop=F]
})

## TODO: Create tests for Level class


#
# l1 <- LevelContinuous(bins=list(a, BinMissing()))
# l2 <- LevelContinuous(bins=list(
#   BinNumeric(lower=5, upper=20),
#   BinNumeric(lower=30, upper=40),
#   BinNumeric(lower=35, upper=50)
#
# ))
# l3 <- LevelContinuous(bins=list(
#   BinNumeric(lower=60, upper=100)
# ))
# l4 <- LevelContinuous(bins=list(
#   BinException(exception=-1),
#   BinMissing()
# ))
#
#
# combine(l1, l2)
#
# #
# arg1 <- list(l1, l2, l3)
#
# # Reduce(combine, arg1[2:3])
# #
# combine(arg1, l4)
#
#
#
