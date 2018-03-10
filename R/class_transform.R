#' @include class_level.R generics.R

## Transform class to hold a list of levels
## Has predict method, dense and sparse indicators

## TODO: Add helpful messages to validity checks

Transform <- setClass("Transform", slots = list(
  levels = "list"
), contains = "VIRTUAL")

setGeneric("add_level", def = function(object, level, ...) standardGeneric("add_level"))

TransformContinuous <- setClass("TransformContinuous", contains = "Transform")

setValidity(
  "TransformContinuous",
  method = function(object) {
    result <- all(vapply(object@levels, is, TRUE, "LevelContinuous"))
    if (result) result else "All levels must be LevelContinuous objects"
  })

TransformDiscrete <- setClass("TransformDiscrete", contains = "Transform")

setValidity(
  "TransformDiscrete",
  method = function(object) {
    result <- all(vapply(object@levels, is, TRUE, "LevelDiscrete"))
    if (result) result else "All levels must be LevelDiscrete objects"
  })

setMethod("len", "Transform", function(x) length(x@levels))

setMethod(
  "add_level",
  signature = c("TransformContinuous", "LevelContinuous"),
  definition = function(object, level, ...) {
    object@levels <- append(object@levels, level)
    object
  })

setMethod(
  "add_level",
  signature = c("TransformDiscrete", "LevelDiscrete"),
  definition = function(object, level, ...) {
    object@levels <- append(object@levels, level)
    object
  })

setMethod(
  "get_exceptions",
  signature = c("TransformContinuous"),
  definition = function(object, ...) {
    do.call(c, lapply(object@levels, get_exceptions))
  })

## general looping function for prediction to DRY
## TODO: DOCUMENT
predict_internal_ <- function(out, object, x, mask=FALSE, FUN) {
  for (i in seq_along(object@levels)) {
    l <- object@levels[[i]]
    f <- get_boolean_mask(l, x, mask=mask)
    if (is.list(out)) out[[i]] <- FUN(f, l) else out[f] <- FUN(f, l)
  }
  return(out)
}

setMethod(
  "predict",
  signature = "Transform",
  function(object, x, type="value", mask=FALSE, ...) {
    switch(type,

           "label" = {
             out <- rep("NULL", length(x))
             predict_internal_(out, object, x, mask, function(f, l) get_label(l))
           },

           "sparse" = {
             out <- vector("list", length(object@levels))
             dims <- c(length(x), 1L)
             out <- predict_internal_(out, object, x, mask, function(f, l) {
               sparseMatrix(i=which(f), j=rep(1, sum(f)), x=1, dims = dims)
             })
             do.call(cbind, out)
           },

           { ## Default
             out <- rep(NaN, (length(x)))
             predict_internal_(out, object, x, mask, function(f, l) l@values[[type]])
           })
  })

setMethod(
  "predict",
  signature = "TransformContinuous",
  function(object, x, type="value", ...) {
    stopifnot(is.numeric(x))
    mask <- x %in% get_exceptions(object)
    callNextMethod(object, x, type, mask=mask)
  })

setMethod(
  "predict",
  signature = "TransformDiscrete",
  function(object, x, type="value", ...) {
    stopifnot(is.factor(x))
    callNextMethod()
  })


setGeneric("make_Transform", def = function(x, addMissing=TRUE, exceptions=numeric(0)) standardGeneric("make_Transform"))

setMethod(
  "make_Transform",
  signature = "character",
  definition = function(x, addMissing=TRUE, exceptions) {
    tf <- TransformDiscrete()

    for (level in x) {
      l <- LevelDiscrete(bins=list(BinFactor(level=level)))
      tf <- add_level(tf, l)
    }

    if (addMissing) tf <- add_level(tf, LevelDiscrete(bins=list(BinMissing())))

    validObject(tf, complete = TRUE)
    tf
  }
)

setMethod(
  "make_Transform",
  signature = c("numeric"),
  definition = function(x, addMissing=TRUE, exceptions) {

    tf <- TransformContinuous()
    breaks <- sort(unique(c(-Inf, x, Inf)))

    for (i in seq.int(length(breaks) - 1)) {
      l <- LevelContinuous(bins=list(
        BinNumeric(lower=breaks[[i]], upper=breaks[[i+1]])))
      tf <- add_level(tf, l)
    }

    for (i in seq_along(exceptions)) {
      tf <- add_level(tf, LevelContinuous(bins=list(BinException(exception=exceptions[[i]]))))
    }

    if (addMissing) tf <- add_level(tf, LevelContinuous(bins=list(BinMissing())))

    validObject(tf, complete = TRUE)
    tf
  })


## assign named list to level values
setMethod(
  "value<-",
  signature = c("Transform", "list"),
  definition = function(object, value) {

    if (is.null(names(value))) stop("value must be a named list")

    ll <- do.call(mapply, c(value, list, SIMPLIFY=FALSE)) ## list of lists

    if (!identical(len(object), length(ll))) {
      stop("length of elements in \"value\" must match number of transform levels")
    }

    for (i in seq_along(ll)) {
      value(object@levels[[i]]) <- ll[[i]]
    }

    validObject(object)
    object
  })

setMethod(
  "show",
  signature = "Transform",
  definition = function(object) {

    for (i in seq_along(object@levels)) {
      l <- object@levels[[i]]
      cat(sprintf("%2d: ", i), sep = "")
      show(l)
      cat("", sep = "\n")
    }

  })


#
#
#
# value(n) <- list(value=1:5, butt=letters[6:10])

# predict(n, factor(letters), type="butt")


#
#
# predict(b, factor(letters), type="sparse")
# predict(b, factor(letters), type="sparse")
#
#
# ## this is how to do collapsing ... Not quite
# n <- make_Transform(seq(25, 75, 25))
# ## test collapse
# i <- c(1,5)
#
#
# combined <- combine(n@levels[i])
# rest <- n@levels[-i]
#
# v <- do.call(rbind, Map(ordervalue, c(combined, rest)))
# i <- order(v[,1], v[,2])
#
#
# TransformDiscrete(levels=c(combined, rest)[i])
# #
# #
# #
# #
# # #
# # # combine(b@levels[1:3])
# # #
# # # value(b) <- c(1:3, NA)
# # #
# # # predict(b, factor("c"), type="label")
# # #
# # #n

#n <- make_Transform(seq(2, 10, 2))
#value(n) <- list(value=seq.int(len(n)), value2=sample(letters, (len(n))))

#predict(n, 1:100, "label")
#
# ## list of values
# value <- list(value=1:5, woe=6:20)
# value(n) <- list(value=1:5, woe=6:20)
#
# do.call(mapply, c(value, list, SIMPLIFY=FALSE))
#
# v <- list(value=1:10, woe=11:20)
# ## get list of named lists
#
# ll <- mapply(list, v)
#
#
