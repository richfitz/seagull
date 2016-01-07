context("fcntl")

test_that("simple", {
  f <- tempfile()
  handle <- seagull_open(f)
  on.exit(file_remove(f))

  expect_true(fcntl_lock(handle, TRUE))
  expect_true(fcntl_lock(handle, TRUE))
  expect_true(fcntl_lock(handle, FALSE))
  expect_true(fcntl_lock(handle, FALSE))

  res <- fcntl_state(handle)
  expect_equal(fcntl_state(handle),
               list(locked=FALSE, pid=Sys.getpid()))

  seagull_close(handle)
  expect_error(seagull_close(handle), "File handle already closed")
  rm(handle)
  gc()
})

test_that("Parallel", {
  fn <- tempfile()
  fh <- seagull_open(fn)
  expect_true(fcntl_lock(fh, TRUE))

  cl <- start_cluster(1)
  on.exit(stop_cluster(cl))

  expect_equal(parallel::clusterCall(cl, f_remote, fn, "state")[[1]],
               list(locked=TRUE, pid=Sys.getpid()))
  tmp <- parallel::clusterCall(cl, f_remote, fn, "open")[[1]]
  expect_false(as.logical(tmp))
  expect_equal(attr(tmp, "errno"), 11L)
})
