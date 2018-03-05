a <- BinNumeric(lower=1.0212, upper=10.0)
b <- BinFactor(level="CAR WASH")

get_label(a)

get_label(b)


l <- LevelContinuous(bins=list(b, b))


get_label(l)


value(l) <- "AT THE"
get_boolean_mask(l, factor(c("CAR WASH", "BAR BASH")))




l <- LevelContinuous(bins=list(a))
l <- add_bin(l, BinMissing())
l <- add_bin(l, BinException(exception=-1))

e <- BinException(exception=1:10)

#l <- add_bin(l, BinFactor(level="BISTRO"))

x <- c(-1, 0, 10, -1, 100, 100, 100, NA, NA, 5, 5, -1, 10)
get_boolean_mask(l, x)
