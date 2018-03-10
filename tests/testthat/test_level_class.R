
context("Level Class")


test_that("Level constructors", {

  expect_s4_class(LevelContinuous(), "LevelContinuous")
  expect_s4_class(LevelContinuous(bins=list(
    BinNumeric(),
    BinMissing(),
    BinException()
  )), "LevelContinuous")

  expect_equal(
    LevelContinuous(bins=list(
      BinNumeric(lower=0, upper=10),
      BinNumeric(lower=10, upper=20))),
    LevelContinuous(bins=list(
      BinNumeric(lower=0, upper=20))),
    label = "Automatically combine overlapped bins")

  expect_equal(
    LevelContinuous(bins=list(
      BinNumeric(lower=0, upper=10),
      BinNumeric(lower=0, upper=10))),
    LevelContinuous(bins=list(
      BinNumeric(lower=0, upper=10))),
    label = "Automatically dedup identical bins")

  expect_s4_class(LevelDiscrete(), "LevelDiscrete")

  expect_error(LevelDiscrete(bins=list(
    BinFactor(),
    BinMissing(),
    BinNumeric()
  )))

  expect_equal(
    LevelDiscrete(bins=list(
      BinFactor(level="cat"),
      BinFactor(level="cat"))),
    LevelDiscrete(bins=list(
      BinFactor(level="cat")))
  )


})

test_that("Level Labels", {

  A <- LevelContinuous(bins=list(BinNumeric(lower=0, upper=10)))
  B <- LevelContinuous(bins=list(BinMissing()))
  C <- LevelDiscrete(bins=list(BinFactor(level="dog"), BinMissing()))

  expect_equal(trimws(capture_output(trimws(show(A)))), "(0 - 10] => NaN")
  expect_equal(trimws(capture_output(trimws(show(B)))), "Missing => NaN")
  expect_equal(trimws(capture_output(trimws(show(C)))), "dog, Missing => NaN")

  D <- add_bin(A, BinException(exception=-1))
  D <- add_bin(D, BinMissing())
  expect_equal(trimws(capture_output(trimws(show(D)))), "(0 - 10], -1, Missing => NaN")

  D <- add_bin(D, BinNumeric(lower=5, upper=20))
  expect_equal(trimws(capture_output(trimws(show(D)))), "(0 - 20], -1, Missing => NaN")

})


test_that("Sort Levels", {

  expect_equal(
    LevelContinuous(bins=list(
      BinNumeric(lower=0, upper=10),
      BinNumeric(lower=10, upper=20))),
    LevelContinuous(bins=list(
      BinNumeric(lower=0, upper=20)))
    )

  expect_equal(
    LevelContinuous(bins=list(
      BinNumeric(lower=10, upper=20),
      BinNumeric(lower=0, upper=10))),
    LevelContinuous(bins=list(
      BinNumeric(lower=0, upper=20)))
    )
})

test_that("Combine Levels", {

  A <- LevelContinuous(bins=list(BinNumeric(lower=0, upper=10)))
  B <- LevelContinuous(bins=list(BinMissing()))
  C <- LevelDiscrete(bins=list(BinFactor(level="dog"), BinMissing()))
  D <- LevelContinuous(
    bins=list(BinNumeric(lower=0, upper=10),
              BinMissing()))
  E <- LevelContinuous(bins=list(BinException(exception=-1)))

  expect_equal(combine(A, B), D)
  expect_error(combine(A, C))
  expect_equal(combine(B, B), B)
  expect_equal(combine(list(B, B, B, B)), B)
  expect_equal(combine(list(A, B, D)), D)
  expect_error(combine(C, E))

  expect_equal(combine(list(A, B)), combine(list(A, B)))

  expect_equal(combine(list(A, B), D), combine(D, list(A, B)))

})


test_that("Order Values", {

  A <- LevelContinuous(bins=list(BinNumeric(lower=0, upper=10)))
  B <- LevelContinuous(bins=list(BinMissing()))
  C <- LevelDiscrete(bins=list(BinFactor(level="dog"), BinMissing()))
  D <- LevelContinuous(bins=list(
    BinNumeric(lower=0, upper=10),
    BinMissing()))
  E <- LevelContinuous(bins=list(BinException(exception=-1)))
  G <- LevelContinuous(bins=list(
    BinNumeric(lower=0, upper=10),
    BinNumeric(lower=-10, upper=0),
    BinException(exception=-1),
    BinMissing()
  ))

  expect_equal(ordervalue(A), matrix(c(1, 0), 1))
  expect_equal(ordervalue(B), matrix(c(3, Inf), 1))
  expect_equal(ordervalue(C), matrix(c(1, as.numeric(charToRaw("d"))), 1))
  expect_equal(ordervalue(D), matrix(c(1, 0), 1))
  expect_equal(ordervalue(E), matrix(c(2, -1), 1))
  expect_equal(ordervalue(G), matrix(c(1, -10), 1))


})



