#' @include class_bins.R

## A level maintains a list of bins
## has operations for combining them
## a value assignment mapping as well
## A label
## add bins
## remove bins
setClass("Level", slots = c(bins="list", value="ANY", label="character"), contains="VIRTUAL")

## differen
LevelContinuous <- setClass("LevelContinuous", contains="Level")

LevelDiscrete <- setClass("LevelDiscrete", contains="Level")

setMethod(
  "get_boolean_mask",
  signature = c("Level", "ANY"),
  definition = function(object, x, ...) {
    Reduce(`|`, lapply(object@bins, get_boolean_mask, x))
  })


setGeneric("value<-", def = function(object, value) standardGeneric("value<-"))

setMethod(
  "value<-",
  signature = c("Level", "ANY"),
  definition = function(object, value) {
    object@value <- value
    object
  }
)

setMethod(
  "get_label",
  signature = c("Level"),
  definition = function(object, ...) {
    do.call(paste, c(lapply(object@bins, get_label, ...), sep=", "))
  })