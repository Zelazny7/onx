#' @include class_transform.R

## performance class

## TODO:: thing about adding a valid perf flag stored with this object?
## Could potentially allow for multiple performances ...

# Can discretize?
# takes x, w, y
#

Perf <- setClass("Perf", list(y="vector", w="numeric", valid="logical"), contains="VIRTUAL")

setMethod(
  "initialize",
  "Perf",
  function(.Object, y, w) {
    .Object@y <- y
    .Object@w <- if (missing(w)) rep(1, length(y)) else w
    stopifnot(identical(length(.Object@y), length(.Object@w)))
    .Object@valid <- !is.na(y)
    validObject(.Object)
    .Object
  }
)


setMethod("summarize", c("Perf", "logical"), function(object, by, ...) stop("Must Implement"))



PerfBinary <- setClass("PerfBinary", list(y="integer"), contains = "Perf")
PerfContinuous <- setClass("PerfContinuous", list(y="numeric"), contains = "Perf")



setMethod(
  "summarize",
  c("PerfBinary", "logical"),
  function(object, by=TRUE, totals, ...) {
    
    f <- by & object@valid
    
    ## create a filter for valid records to summarize
    if (missing(totals)) totals <-
        c("#0"=sum(object@w[f & object@y == 0]),
          "#1"=sum(object@w[f & object@y == 1]))
    
    ## create summary pieces
    N <- sum(object@w[f])
    `#1` <- sum(object@w[f & object@y == 1])
    `#0` <- (N - `#1`)
    `%N` <- N / (totals[["#1"]] + totals[["#0"]])
    `%1` <- `#1` / totals[["#1"]]
    `%0` <- `#0` / totals[["#0"]]
    `P(1)` <- `#1` / N
    WoE <- log(`%1` / `%0`)
    IV <- WoE * (`%1` - `%0`)
    
    ## put it all in a data.frame
    data.frame(N, `#1`, `#0`, `%N`, `%1`, `%0`, `P(1)`, WoE, IV, check.names = FALSE)
    
  }
)

setMethod(
  "summarize",
  c("PerfContinuous", "logical"),
  function(object, by=TRUE, totals, ...) {
    
    f <- by & object@valid
    
    wtd <- object@w[f] * object@y[f]
    
    ## create a filter for valid records to summarize
    if (missing(totals)) totals <-c("N"=sum(object@w[f]), "sum"=sum(wtd))
    
    ## create summary pieces
    N <- sum(object@w[f])
    `%N` <- N / totals[["N"]]
    sum <- sum(wtd)
    `%sum` <- sum / totals[["sum"]]
    mean <- sum/N
    var <- sum(object@w[f] * ((object@y[f] - mean)^2))/N
    
    ## put it all in a data.frame
    data.frame(N, `%N`, sum, `%sum`, mean, var, check.names = FALSE)
    
  }
)




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
