a <- BinNumeric(lower=1.0212, upper=10.0)
b <- BinFactor(level="CAR WASH")

get_label(a)

get_label(b)


l <- LevelContinuous(bins=list(b, b))


get_label(l)


value(l) <- "AT THE"
get_boolean_mask(l, factor(c("CAR WASH", "BAR BASH")))
