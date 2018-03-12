#n <- make_Transform(seq(2, 10, 2))
#value(n) <- list(value=seq.int(len(n)), value2=sample(letters, (len(n))))

#x <- sample(1:12, 1.5e5, T)
#p <- predict(n, x, type="value2")
#table(x, p)

data(titanic, package="onyx")
x <- titanic$Fare
y <- titanic$Survived
w <- rep(1, length(x))

## create the transform
tf <- make_Transform(quantile(x, seq(0.2, 0.8, 0.2)), addMissing=TRUE, exceptions=-1)

v <- new("VariableContinuous", tf=tf, x=x)

tf <- make_Transform(levels(titanic$Pclass), addMissing=TRUE)
v <- new("VariableDiscrete", tf=tf, x=titanic$Pclass)


p <- factor(predict(tf, x, "label"), levels=sapply(tf@levels, get_label))

classing <- list()
for (v in names(titanic[-1])) {
  x <- titanic[[v]]
  if (is.factor(x)) {
    classing[[v]] <- make_Transform(levels(x), addMissing = TRUE)
  } else {
    q <- quantile(x, seq(0.2, 0.8, 0.2), na.rm = T)
    classing[[v]] <- make_Transform(q, addMissing = TRUE)
  }
}


X <- mapply(predict, classing, titanic[-1], MoreArgs = list(type="sparse"), SIMPLIFY = F)
X <- do.call(cBind, X)

library(glmnet)

mod <- cv.glmnet(X, y = titanic$Survived, family="binomial", alpha=0)

p <- predict(mod, X, lambda="lambda.1se")

plot(pROC::roc(titanic$Survived, -p[,1]))
#lapply(titanic[-1], make_Transform)





pf <- PerfBinary(y=as.integer(titanic$Survived))
dc <- QuantileDiscretizer()

tf <- discretize(pf, dc, x=titanic$Age, w=rep(1, nrow(titanic)))





