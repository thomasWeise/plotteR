library("plotteR")
context("distinctColors")

test_that("Test colors.distinct 1 to 50", {
  for(i in 1:50) {
    colors <- colors.distinct(i);
    expect_length(colors, i);
    expect_length(unique(colors), i);
    for(c in colors) {
      expect_is(c, "character");
      expect_true(nchar(c) > 0);
    }
    expect_identical(colors, colors.distinct(i));
  }
})

test_that("Test colors.distinct 100", {
  i <- 100;
  colors <- colors.distinct(i);
  expect_length(colors, i);
  expect_length(unique(colors), i);
  for(c in colors) {
    expect_is(c, "character");
    expect_true(nchar(c) > 0);
  }
  expect_identical(colors, colors.distinct(i));
})

test_that("Test colors.distinct 200", {
  i <- 200;
  colors <- colors.distinct(i);
  expect_length(colors, i);
  expect_length(unique(colors), i);
  for(c in colors) {
    expect_is(c, "character");
    expect_true(nchar(c) > 0);
  }
  expect_identical(colors, colors.distinct(i));
})
