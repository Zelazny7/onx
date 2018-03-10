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
