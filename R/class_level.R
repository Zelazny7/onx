#' @include class_bins.R generics.R

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
setClass("Level", slots = c(bins="list", values="list"), contains="VIRTUAL")

LevelContinuous <- setClass("LevelContinuous", contains="Level")

setValidity(
  "LevelContinuous",
  function(object) {
    all(vapply(object@bins, is, TRUE, "LevelContinuousAppendable"))
  })

setMethod(
  "initialize",
  signature = "Level",
  function(.Object, bins=list(), values=list(Value=NaN), ...) {
    .Object@values <- list(value=NaN)
    .Object@bins <- bins[!duplicated(bins)] ## No duplicated bins
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
    
    cat(sprintf("%20s => %s", get_label(object), object@values[["value"]]))
    
  })

################# Combine/Expand ----

## combine-Level,Level ----
#' @rdname combine-methods
#' @aliases combine,Level,Level-method
setMethod(
  "combine",
  signature = c("Level", "Level"),
  definition = function(a, b) {
    
    bins <- Reduce(combine, c(a@bins, b@bins))
    if (!is.list(bins)) bins <- list(bins)
    
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
