library("plotteR")
context("plot.size")

test_that("Test plots.size", {
  for(i in 1:50) {
    for(j in 1:50) {
      res <- plots.size(i, j);
      expect_length(res, 2L);
      expect_equal(round(10L*res)/10L, res);
      expect_true(all(res > 0L));
      expect_true(all(is.finite(res)));
      expect_gte(res[1] * res[2], 1);
    }
  }
})
