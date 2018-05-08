#' @include class_performance.R

## define discretizers for various performance metrics here

setClass("Discretizer", contains = "VIRTUAL")

QuantileDiscretizer <- setClass("QuantileDiscretizer", contains = "Discretizer")
IVDiscretizer <- setClass("IVDiscretizer", contains = "Discretizer")
VarianceDiscretizer <- setClass("VarianceDiscretizer", contains = "Discretizer")

setGeneric("discretize", def = function(x, perf, discretizer, ...) standardGeneric("discretize"))

#' @importFrom Hmisc wtd.quantile
setMethod(
  "discretize",
  "signature" = c(x="numeric", perf="Perf"),
  definition = function(x, perf, discretizer, probs=seq(0.2, 0.8, 0.2), ...) {
    breaks <- Hmisc::wtd.quantile(x, perf@w, probs, na.rm = TRUE)
    tf <- make_Transform(breaks, ...)
    tf
  })
