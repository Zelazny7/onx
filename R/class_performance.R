## performance class

## TODO:: thing about adding a valid perf flag stored with this object?
## Could potentially allow for multiple performances ...

# Can discretize?
# takes x, w, y
#

Perf <- setClass("Perf", list(y="vector"), contains="VIRTUAL")

setGeneric("discretize", def = function(perf, discretizer, x, w, ...) standardGeneric("discretize"))

setClass("Discretizer", contains = "VIRTUAL")

QuantileDiscretizer <- setClass("QuantileDiscretizer", contains = "Discretizer")
IVDiscretizer <- setClass("IVDiscretizer", contains = "Discretizer")
VarianceDiscretizer <- setClass("VarianceDiscretizer", contains = "Discretizer")

#' @importFrom Hmisc wtd.quantile
setMethod(
  "discretize",
  "signature" = c("Perf", "QuantileDiscretizer"),
  definition = function(perf, discretizer, x, w=rep(1, length(x)), probs=seq(0.2, 0.8, 0.2), ...) {
    breaks <- Hmisc::wtd.quantile(x, w, probs, na.rm = TRUE)
    tf <- make_Transform(breaks, ...)
    tf
  })


setMethod("discretize", def = function(perf, discretizer, ...) standardGeneric("discretize"))

PerfBinary <- setClass("PerfBinary", list(y="integer"), contains = "Perf")




# y <- Perf(y=titanic$Survived)
#
# classing <- vector("list", length = 100)
#
#
# object.size(classing)
#
# for (i in seq_along(classing)) {
#   classing[[i]] <- y
# }
#
# classing[[i]]@y <- titanic$Fare
