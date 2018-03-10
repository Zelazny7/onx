
context("Level Class")


test_that("Level constructors", {

  expect_s4_class(LevelContinuous(), "LevelContinuous")
  expect_s4_class(LevelContinuous(bins=list(
    BinNumeric(),
    BinMissing(),
    BinException()
  )), "LevelContinuous")

  expect_s4_class(LevelDiscrete(), "LevelDiscrete")

  expect_error(LevelDiscrete(bins=list(
    BinFactor(),
    BinMissing(),
    BinNumeric()
  )))


})

