## R6 class that manages
## Read up on R6 classes
## Read new book on OOP in R


Variable <- setRefClass("Variable", fields = c(tf="Transform", x="vector"), contains="VIRTUAL")

Variable$methods(
  ## assign a named list of values to the transform
  set_values = function(l) {
    value(tf) <<- l
  },
  predict = function(type="value", ...) {
    do.call("predict", list(tf, x, type), envir = sys.frame())
  },
  factorize = function() {
    factor(predict("label"), levels=sapply(tf@levels, get_label))
  })


Variable$methods(summarize = function(perf) {
  
  # get the boolean masks from the transform
  masks <- get_boolean_mask(tf, x)
  
  ## get the totals for the perf
  totals <- onx::summarize(perf, by=TRUE)
  
  bands <- lapply(masks, function(mask) onx::summarize(perf, by=mask, totals=totals))
  
  c(bands, list(totals))
  
})

VariableContinuous <- setRefClass("VariableContinuous", fields = c(tf="TransformContinuous", x="numeric"), contains="Variable")

VariableDiscrete <- setRefClass("VariableDiscrete", fields = c(tf="TransformDiscrete", x="factor"), contains="Variable")

