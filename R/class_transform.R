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
  })

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

setMethod(
  "get_values",
  signature = c("Transform", "character"),
  definition = function(object, values, ...) {
    setNames(lapply(object@levels, get_values, values), get_label(object))
  })

setMethod(
  "get_values",
  signature = c("Transform", "missing"),
  definition = function(object, values, ...) {
    setNames(lapply(object@levels, get_values), get_label(object))
  })

setMethod(
  "get_label",
  signature = c("Transform"),
  definition = function(object, ...) {
    vapply(object@levels, get_label, "")
  })

### TODO: Add collapse, expand
setMethod("collapse", c("Transform", "numeric"), function(object, i) collapse(object, as.integer(i)))

setMethod(
  "collapse",
  signature = c("Transform", "integer"),
  definition = function(object, i) {

    if (!all(i %in% seq.int(len(object)))) {
      warning("invalid range for collapse requested")
      return(object)
    }

    collapsed <- combine(object@levels[i])
    out <- new(class(object), levels=c(collapsed, object@levels[-i]))
    validObject(out)
    out

  })


