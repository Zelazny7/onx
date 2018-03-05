## variable class is just a container object
# variables -- Has Performance
#  |- levels
#     |- bins
#

## S3?

## bin interface
## constuctor
## u
## predict
##
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

BinFactor <- setClass("BinFactor", slots = list(
  level = "character"),
  prototype = list(
    level = ""
  ),
  contains = "Bin"
)

BinMissing <- setClass("BinMissing", contains = "Bin")

setGeneric("get_boolean_mask", function(object, x, ...) standardGeneric("get_boolean_mask"))

setMethod(
  "get_boolean_mask",
  signature = c("BinNumeric", "numeric"),
  definition = function(object, x, ...) {
    x >= object@lower & x < object@upper & !is.na(x)
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


## A level maintains a list of bins
## has operations for combining them
## a value assignment mapping as well
## A label
## add bins
## remove bins
setClass("Level", slots = c(bins="list", value="ANY", label="character"), contains="VIRTUAL")

## differen
setClass("LevelContinuous", contains="Level")

setClass("LevelDiscrete", contains="Level")

setMethod(
  "get_boolean_mask",
  signature = c("Level", "ANY"),
  definition = function(object, x, ...) {
    Reduce(`|`, lapply(object@bins, get_boolean_mask, x))
  })

setGeneric("set_level_value", def = function(object, value) standardGeneric("set_level_value"))

setMethod(
  "set_level_value",
  signature("")
)


a <- BinNumeric(lower=7, upper=20)
b <- BinNumeric(lower=5, upper=10)

l <- Level(bins=list(a, b))

get_boolean_mask(l, 1:100)


