a <- BinNumeric(lower=1.0212, upper=10.0)
b <- BinNumeric(lower=11, upper=20)



b <- BinNumeric(lower=11, upper=20)
expand(b, breaks = c(13))



c <- BinMissing()
d <- BinFactor(level="z")

b <- BinNumeric(lower=11, upper=20)


combine(a, d)


b <- BinFactor(level="CAR WASH")

get_label(a)
get_label(b)


l1 <- LevelContinuous(bins=list(BinNumeric(lower=1, upper=10)))
l2 <- LevelContinuous(bins=list(BinNumeric(lower=10, upper=20)))


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


