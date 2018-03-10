
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




})


test_that("Sort Levels", {

  expect_equal(
    LevelContinuous(bins=list(
      BinNumeric(lower=0, upper=10),
      BinNumeric(lower=10, upper=20))),
    LevelContinuous(bins=list(
      BinNumeric(lower=0, upper=20)))
    )


})
