library("plotteR")
context("plots.arrange")

test_that("Test plots.arrange", {
  for(i in 1:500) {
    res <- plots.arrange(i);
    expect_length(res, 2L);
    expect_true(is.integer(res));
    expect_true(all(res > 0L));
    expect_true(all(is.finite(res)));
    expect_gte(res[1] * res[2], i);
  }
})
