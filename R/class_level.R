#' @include class_bins.R

setClassUnion("LevelContinuousAppendable", c("BinNumeric", "BinMissing", "BinException"))
setClassUnion("LevelDiscreteAppendable", c("BinFactor", "BinMissing"))

## A level maintains a list of bins
## has operations for combining them
## a value assignment mapping as well
## A label
## add bins
## remove bins

### TODO: Need method to reduce or dedupe bins within a level
### TODO: Sort bins within a level
### TODO: Think about dropping the label and just generate it on request
setClass("Level", slots = c(bins="list", value="ANY"), contains="VIRTUAL")

## differen
LevelContinuous <- setClass("LevelContinuous", contains="Level")

setValidity(
  "LevelContinuous",
  function(object) {
    all(vapply(object@bins, is, TRUE, "LevelContinuousAppendable"))
  })

LevelDiscrete <- setClass("LevelDiscrete", contains="Level")

setValidity(
  "LevelDiscrete",
  function(object) {
    all(vapply(object@bins, is, TRUE, "LevelDiscreteAppendable"))
  })


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
  signature = c("LevelContinuous", "LevelContinuousAppendable"),
  definition = function(object, bin) {
    object@bins <- append(object@bins, bin)
    object
  })

setMethod(
  "add_bin",
  signature = c("LevelDiscrete", "LevelDiscreteAppendable"),
  definition = function(object, bin) {
    object@bins <- append(object@bins, bin)
    object
  })
