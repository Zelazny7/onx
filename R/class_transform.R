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

TransformDiscrete <- setClass("TransformDiscrete", slots=list(factor_levels="character"), contains = "Transform")

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


## get the indices of each?
get_level_indices_ <- function(tf, x) {
  mask <- x %in% get_exceptions(tf)
  index <- integer(length(x))
  for (i in seq_along(tf@levels)) {
    l <- tf@levels[[i]]
    f <- get_boolean_mask(l, x, mask=mask)
    index[f] <- i
  }
  index
}


## TODO: DOCUMENT THIS!
setMethod(
  "predict",
  signature = "Transform",
  function(object, x, type="value", ...) {
    
    i <- get_level_indices_(object, x)
    
    switch(type,

           "label" = get_label(object)[i],

           "sparse" = {
              if (any(i == 0)) stop("Factor levels not found and no missing fallback!")
              sparseMatrix(i=seq_along(i), j = i, x = 1, dims = c(length(i), len(object)))
             },
           
           { ## Default
             v <- unlist(values(object, type), F, F)
             unlist(v[i], F, F)
           })
  })


setMethod(
  "predict",
  signature = "TransformContinuous",
  function(object, x, type="value", ...) {
    stopifnot(is.numeric(x))
    callNextMethod()
  })


setMethod(
  "predict",
  signature = "TransformDiscrete",
  function(object, x, type="value", ...) {
    stopifnot(is.factor(x))
    x <- factor(x, levels=object@factor_levels)
    callNextMethod(object=object, x=x, type=type, ...)
  })


setGeneric("make_Transform", def = function(x, addMissing=TRUE, exceptions=numeric(0)) standardGeneric("make_Transform"))

setMethod(
  "make_Transform",
  signature = "character",
  definition = function(x, addMissing=TRUE, exceptions) {
    tf <- TransformDiscrete(factor_levels=x)

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

## Return the requested value in a list
## Multiple requested values are stored in lists of lists
setMethod(
  "values",
  signature = c("Transform", "character"),
  definition = function(object, value) {
    
    out <- list()
    for (v in value) {
      out[[v]] <- setNames(lapply(object@levels, values, v), get_label(object))
    }
    
    return(out)
    
  })

## Return ALL values for each level in a list of lists
setMethod(
  "values",
  signature = c("Transform", "missing"),
  definition = function(object, value) {
    value <- names(object@levels[[1]]@values)
    callGeneric(object, value)
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


