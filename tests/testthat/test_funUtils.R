library("plotteR")
context("funUtils")

test_that("Test .make.findFun", {
  data <- 1L:10L;
  test <- unique(c(data, data + 0.5, data - 0.5));

  for(comparator in c(`<=`, `<`, `>`, `>=`)) {
    findFun <- .make.findFun(comparator);
    expect_identical(findFun, .make.findFun(comparator));

    data <- sort(data, decreasing=comparator(0L, 1L));

    for(t in test) {
      i <- findFun(t, data);

      if(i <= 0) {
        expect_false(any(comparator(data, t)));
      } else {
        if(i > 1L) {
          expect_false(comparator(data[i - 1L], t));
        }
        expect_true(comparator(data[i], t));
      }
    }
  }
})


test_that("Test .dim.min", {
  d1 <- c(1, 2);
  d2 <- c(2, 3);
  d3 <- matrix(c(1, 2, 3, 4, 3, 2, 1, 0), ncol=2L);

  expect_identical(.dim.min(list(d1), 1L, as.integer), 1L);
  expect_identical(.dim.min(list(d1), 2L, as.integer), 2L);
  expect_identical(.dim.min(list(d1), 1L, identity), 1);
  expect_identical(.dim.min(list(d1), 2L, identity), 2);

  expect_identical(.dim.min(list(d2), 1L, as.integer), 2L);
  expect_identical(.dim.min(list(d2), 2L, as.integer), 3L);
  expect_identical(.dim.min(list(d2), 1L, identity), 2);
  expect_identical(.dim.min(list(d2), 2L, identity), 3);

  expect_identical(.dim.min(list(d1, d2), 1L, as.integer), 1L);
  expect_identical(.dim.min(list(d1, d2), 2L, as.integer), 2L);
  expect_identical(.dim.min(list(d1, d2), 1L, identity), 1);
  expect_identical(.dim.min(list(d1, d2), 2L, identity), 2);

  expect_identical(.dim.min(list(d2, d1), 1L, as.integer), 1L);
  expect_identical(.dim.min(list(d2, d1), 2L, as.integer), 2L);
  expect_identical(.dim.min(list(d2, d1), 1L, identity), 1);
  expect_identical(.dim.min(list(d2, d1), 2L, identity), 2);

  expect_identical(.dim.min(list(d1, d2, d3), 1L, as.integer), 1L);
  expect_identical(.dim.min(list(d1, d2, d3), 2L, as.integer), 0L);
  expect_identical(.dim.min(list(d1, d2, d3), 1L, identity), 1);
  expect_identical(.dim.min(list(d1, d2, d3), 2L, identity), 0);
})



test_that("Test .dim.max", {
  d1 <- c(1, 2);
  d2 <- c(2, 3);
  d3 <- matrix(c(1, 2, 3, 4, 3, 2, 1, 0), ncol=2L);

  expect_identical(.dim.max(list(d1), 1L, as.integer), 1L);
  expect_identical(.dim.max(list(d1), 2L, as.integer), 2L);
  expect_identical(.dim.max(list(d1), 1L, identity), 1);
  expect_identical(.dim.max(list(d1), 2L, identity), 2);

  expect_identical(.dim.max(list(d2), 1L, as.integer), 2L);
  expect_identical(.dim.max(list(d2), 2L, as.integer), 3L);
  expect_identical(.dim.max(list(d2), 1L, identity), 2);
  expect_identical(.dim.max(list(d2), 2L, identity), 3);

  expect_identical(.dim.max(list(d1, d2), 1L, as.integer), 2L);
  expect_identical(.dim.max(list(d1, d2), 2L, as.integer), 3L);
  expect_identical(.dim.max(list(d1, d2), 1L, identity), 2);
  expect_identical(.dim.max(list(d1, d2), 2L, identity), 3);

  expect_identical(.dim.max(list(d2, d1), 1L, as.integer), 2L);
  expect_identical(.dim.max(list(d2, d1), 2L, as.integer), 3L);
  expect_identical(.dim.max(list(d2, d1), 1L, identity), 2);
  expect_identical(.dim.max(list(d2, d1), 2L, identity), 3);

  expect_identical(.dim.max(list(d1, d2, d3), 1L, as.integer), 4L);
  expect_identical(.dim.max(list(d1, d2, d3), 2L, as.integer), 3L);
  expect_identical(.dim.max(list(d1, d2, d3), 1L, identity), 4);
  expect_identical(.dim.max(list(d1, d2, d3), 2L, identity), 3);
})



test_that("Test find.time", {
  data.g <- 1L:10L;
  data.t <- 1L:10L;
  test <- unique(c(data.g, data.g + 0.5, data.g - 0.5));

  for(comparator in c(`<=`, `<`, `>`, `>=`)) {
    findFun <- .make.findFun(comparator);
    expect_identical(findFun, .make.findFun(comparator));

    data.g <- sort(data.g, decreasing=comparator(0L, 1L));
    data.m <- matrix(c(data.t, data.g), ncol=2L);

    for(t in test) {
      time <- .find.time(data.m, t, 2, 1, NA, as.integer, comparator, findFun);
      if(time < 0L) {
        expect_false(any(comparator(data.g, t)));
        expect_identical(time, -10L);
      } else {
        i <- findInterval(time, data.t);
        if(i > 1L) {
          expect_false(comparator(data.g[i - 1L], t));
        }
        expect_true(comparator(data.g[i], t));
      }
    }
  }
})
