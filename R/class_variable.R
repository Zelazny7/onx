#' @include class_level.R

## Variable class to hold a list of levels
## Has predict method, dense and sparse indicators

## TODO: Add helpful messages to validity checks


### TODO: BIG: Need to rethink exceptions and prediction priority

Variable <- setClass("Variable", slots = list(
  levels = "list"
), contains = "VIRTUAL")

setGeneric("add_level", def = function(object, level, ...) standardGeneric("add_level"))

VariableContinuous <- setClass("VariableContinuous", contains = "Variable")

setMethod(
  "initialize",
  signature = "VariableContinuous",
  definition = function(.Object, levels) {
    ## default constructor?
    .Object@levels <- list(
      LevelContinuous(
        bins=list(BinNumeric())
      ),
      LevelContinuous(
        bins=list(BinMissing()))
    )
    .Object
  })

setValidity(
  "VariableContinuous",
  method = function(object) {
    result <- vapply(object@levels, is, TRUE, "LevelContinuous")
    if (result) result else "All levels must be LevelContinuous objects"
  })

VariableDiscrete <- setClass("VariableDiscrete", contains = "Variable")

setValidity(
  "VariableContinuous",
  method = function(object) {
    result <- vapply(object@levels, is, TRUE, "LevelDiscrete")
    if (result) result else "All levels must be LevelDiscrete objects"
  })

## create a variable builder DSL? That would be pretty cool

setMethod(
  "add_level",
  signature = c("VariableContinuous", "LevelContinuous"),
  definition = function(object, level, ...) {
    object@levels <- append(object@levels, level)
    object
  })

setMethod(
  "predict",
  signature = "VariableContinuous",
  function(object, x, type="value", ...) {
    ## check if x is numeric
    stopifnot(is.numeric(x))

    switch(type,

           "value" = {
             out <- rep(NaN, (length(x)))

             for (i in seq_along(object@levels)) {
               l <- object@levels[[i]]
               f <- get_boolean_mask(l, x)
               out[f] <- l@value
             }
             return(out)
           },

           "label" = {
             out <- rep("NULL", length(x))

             for (i in seq_along(object@levels)) {
               l <- object@levels[[i]]
               f <- get_boolean_mask(l, x)
               out[f] <- get_label(l)
             }
             return(out)
           },

           "sparse" = {
             out <- vector("list", length(object@levels))
             dims <- c(length(x), 1L)

             for (i in seq_along(object@levels)) {
               l <- object@levels[[i]]
               f <- get_boolean_mask(l, x)
               out[[i]] <- sparseMatrix(i=which(f), j=rep(i, sum(f)), x=1, dims = dims)
             }
             do.call(cbind, out)
           })
  })



#library(magrittr)
# v <- VariableContinuous() %>%
#   add_level(LevelContinuous(value=1) %>% add_bin(BinNumeric())) %>%
#   #add_level(LevelContinuous(value=2) %>% add_bin(BinMissing())) %>%
#   add_level(LevelContinuous(value=3) %>% add_bin(BinException(exception=-1)))
#
#
