## R6 class that manages
## Read up on R6 classes
## Read new book on OOP in R


Variable <- setRefClass("Variable", fields = c(tf="Transform", x="vector"), contains="VIRTUAL")

Variable$methods(
  set_values = function(l) {
    value(tf) <<- l
  },
  predict = function(type="value", ...) {
    do.call("predict", list(tf, x, type), envir = sys.frame())
  },
  factorize = function() {
    factor(predict("label"), levels=sapply(tf@levels, get_label))
  })

VariableContinuous <- setRefClass("VariableContinuous", fields = c(tf="TransformContinuous", x="numeric"), contains="Variable")

# VariableContinuous$methods(
#   collapse = function(i) {
#     tf@levels <<- combine(tf@levels[i])
#   }
#)

VariableDiscrete <- setRefClass("VariableDiscrete", fields = c(tf="TransformDiscrete", x="factor"), contains="Variable")

