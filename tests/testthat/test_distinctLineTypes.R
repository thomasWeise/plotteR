library("plotteR")
context("lineTypes.distinct")

test_that("Test lineTypes.distinct 1 to 50", {
  for(i in 1:50) {
    lty <- lineTypes.distinct(i);
    expect_length(lty, i);
    expect_gte(length(unique(lty)), min(i, 6L));
    for(c in lty) {
      expect_is(c, "integer");
      expect_gte(c, 1L);
      expect_lte(c, 6L);
    }
    expect_identical(lty, lineTypes.distinct(i));
  }
})
