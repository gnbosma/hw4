test_that("Rank Correct", {
  expect_equal(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                   data = wine)$rank, 4)
})

test_that("Degrees Freedom Residual Correct", {
  expect_equal(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                   data = wine)$df.residual, 46)
})

test_that("Residuals Match LM output", {
  expect_equal(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                   data = wine)$residuals, lm(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                                              data = wine)$residuals)
})

test_that("Model Match LM output", {
  expect_equal(as.matrix(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                   data = wine)$model), as.matrix(lm(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                                              data = wine)$model))
})

test_that("Check that data element is required", {
  expect_error(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar))
})

test_that("Check that formula element is required", {
  expect_error(lm2(data = wine))
})


