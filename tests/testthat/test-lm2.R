test_that("lm2: test default na.action works", {
  expect_output(print_lm2(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine)))
})

test_that("lm2: test na.impute na.action works", {
  expect_output(print_lm2(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine, na.action = 'impute')))
})

test_that("lm2: test na.impute na.action works", {
  expect_error(print_lm2(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine, na.action = 'fail')))
})

test_that("lm2: Rank Correct", {
  expect_equal(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                   data = wine)$rank, 4)
})

test_that("lm2: Degrees Freedom Residual Correct", {
  expect_equal(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                   data = wine)$df.residual, 44)
})

test_that("lm2: Residuals Match LM output", {
  expect_equal(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                   data = wine)$residuals, lm(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                                              data = wine)$residuals)
})

test_that("lm2: Model Match LM output", {
  expect_equal(as.matrix(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                   data = wine)$model), as.matrix(lm(formula = quality ~ volatile.acidity + citric.acid + residual.sugar,
                                              data = wine)$model))
})

test_that("lm2: Check that data element is required", {
  expect_error(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar))
})

test_that("lm2: Check that formula element is required", {
  expect_error(lm2(data = wine))
})

test_that("summarylm2: Unscaled Covariance Correct", {
  expect_equal(as.matrix(summarylm2(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))$coefficients),
               as.matrix(summary(lm(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))$coefficients))
})

test_that("summarylm2: Unscaled Covariance Correct", {
  expect_equal(summarylm2(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))$r.squared,
               summary(lm(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))$r.squared)
})

test_that("summarylm2: Unscaled Covariance Correct", {
  expect_equal(summarylm2(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))$adj.r.squared,
               summary(lm(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))$adj.r.squared)
})

test_that("summarylm2: Unscaled Covariance Correct", {
  expect_equal(summarylm2(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))$cov.unscaled,
               summary(lm(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))$cov.unscaled)
})


test_that("summarylm2: Fstatistic Correct", {
  expect_equal(summarylm2(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))$fstatistic,
               summary(lm(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))$fstatistic)
})

test_that("print_summarylm2: Expect output", {
  expect_output(print_summarylm2(summarylm2(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine))))
})


test_that("print_lm2: Expect output", {
  expect_output(print_lm2(lm2(formula = quality ~ volatile.acidity + citric.acid + residual.sugar, data = wine)))
})

