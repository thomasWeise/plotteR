library("plotteR")
context("plot.func.ertEcdf")



.tester <- function(data) {
  graphics.off();

  # create destination file
  dest <- tempfile(pattern="plot-test", fileext=".pdf");
  expect_false(file.exists(dest));

  pdf(dest, width=6, height=3);
  plot.func.ertEcdf(data);

  dev.off();

  expect_true(file.exists(dest));
  expect_gt(file.size(dest), 100L);

  # delete temp file
  unlink(dest);
}

test_that("Test plot.func.ertEcdf random runs", {
  data.1 <- list(list(c(1, 2), c(2, 2)), list(c(1, 2)), list(c(3, 2)));
  data.2 <- list(list(c(1, 2), c(2, 2)), list(c(1, 2)), list(c(3, 2)), list(c(6,1), c(5,1)));

  .tester(list(data.1, data.2));
})
