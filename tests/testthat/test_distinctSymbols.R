library("plotteR")
context("symbols.distinct")

test_that("Test symbols.distinct 1 to 50", {
  for(i in 1:50) {
    symbols <- symbols.distinct(i);
    expect_length(symbols, i);
    expect_gte(length(unique(symbols)), min(i, 19L));
    for(c in symbols) {
      expect_is(c, "integer");
      expect_gte(c, 0L);
      expect_lte(c, 18L);
    }
    expect_identical(symbols, symbols.distinct(i));
  }
})
