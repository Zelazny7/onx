#' @include class_bins.R generics.R

setClassUnion("LevelContinuousAppendable", c("BinNumeric", "BinMissing", "BinException"))
setClassUnion("LevelDiscreteAppendable", c("BinFactor", "BinMissing"))

order_mapping <- c(
  "BinNumeric"    = 1,
  "BinFactor"     = 1,
  "BinException"  = 2,
  "BinMissing"    = 3)


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

LevelContinuous <- setClass("LevelContinuous", contains="Level")

setValidity(
  "LevelContinuous",
  function(object) {
    all(vapply(object@bins, is, TRUE, "LevelContinuousAppendable"))
  })

setMethod(
  "initialize",
  signature = "Level",
  function(.Object, bins=list(), value=NaN, ...) {
    .Object@value <- NaN
    .Object@bins <- bins
    .Object
  })


LevelDiscrete <- setClass("LevelDiscrete", contains="Level")

setValidity(
  "LevelDiscrete",
  function(object) {
    all(vapply(object@bins, is, TRUE, "LevelDiscreteAppendable"))
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
    
    cat(sprintf("%20s => %s", get_label(object), object@value))
    
  })

################# Combine/Expand ----

## combine-LevelContinuousAppendable,LevelContinuousAppendable ----
#' @rdname combine-methods
#' @aliases combine,LevelContinuousAppendable,LevelContinuousAppendable-method
setMethod(
  "combine",
  signature = c("Level", "Level"),
  definition = function(a, b) {
    
    bins <- Reduce(combine, c(a@bins, b@bins))
    
    ## sort by type then value within type
    v1 <- unlist(Map(ordervalue, bins))
    v2 <- order(unlist(Map(function(b) order_mapping[[class(b)]], bins)))
    
    i <- order(v1, v2)
    
    new(class(a), bins=bins[i])
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
# 
# 
# l <- c(l1@bins, l2@bins)
# 
# combine(l1, l2)
# 
# l <- c(l1@bins, l2@bins)
# 
# l3 <- LevelDiscrete(bins=list(
#   BinFactor(level="a"),
#   BinMissing()
#   ))
# l4 <- LevelContinuous(bins=list(
#   BinFactor(level="b"),
#   BinFactor(level="c"),
#   BinFactor(level="d")
# ))
# 
# l <- c(l3@bins, l4@bins)
# 
# combine(l3, l4)
