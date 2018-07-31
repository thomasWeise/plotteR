library("plotteR")
context("rgb2gray.luminosity")

test_that("Test rgb2gray.luminosity", {
  expect_identical(rgb2gray.luminosity(0, 0, 0), 0);
  expect_identical(rgb2gray.luminosity(1, 1, 1), 1);
  expect_identical(rgb2gray.luminosity(255, 255, 255), 255);

  expect_identical(rgb2gray.luminosity(255, 0, 0), 54.213);
  expect_equal(rgb2gray.luminosity(0, 255, 0), 182.376);
  expect_identical(rgb2gray.luminosity(0, 0, 255), 18.411);
})
