context("utils")

test_that("retry", {
  crit <- 0.1
  make_f <- function() {
    cnt <- 1L
    function() {
      x <- runif(1)
      if (x < crit) {
        list(cnt, x)
      } else {
        cnt <<- cnt + 1L
        stop(RetryAgain())
      }
    }
  }

  res <- replicate(10, retry(make_f(), 0), simplify=FALSE)

  expect_gt(max(vapply(res, function(x) x[[1]], integer(1))), 1)
  expect_lt(min(vapply(res, function(x) x[[2]], numeric(1))), crit)
})
