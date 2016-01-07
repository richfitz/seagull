context("fcntl")

test_that("simple", {
  f <- tempfile()
  handle <- seagull_open(f)
  on.exit(file_remove(f))

  ## OK, this is different between platforms.  Probably I should save
  ## this information with the handle.
  expect_true(fcntl_lock(handle, TRUE))
  expect_equal(fcntl_lock(handle, TRUE)[[1]], !is_windows())
  expect_true(fcntl_lock(handle, FALSE))
  expect_equal(fcntl_lock(handle, FALSE)[[1]], !is_windows())

  if (is_windows()) {
    expect_error(fcntl_state(handle), "Not available")
  } else {
    res <- fcntl_state(handle)
    expect_equal(fcntl_state(handle),
                 list(locked=FALSE, pid=Sys.getpid()))
  }

  expect_true(seagull_close(handle))
  expect_error(seagull_close(handle), "File handle already closed")
  rm(handle)
  gc()
})

test_that("Parallel", {
  fn <- tempfile()
  fh <- seagull_open(fn)
  expect_true(fcntl_lock(fh, TRUE))

  cl <- start_cluster(1L)
  on.exit(stop_cluster(cl))

  if (!is_windows()) {
    expect_equal(parallel::clusterCall(cl, f_remote, fn, "state")[[1]],
                 list(locked=TRUE, pid=Sys.getpid()))
  }
  tmp <- parallel::clusterCall(cl, f_remote, fn, "open")[[1]]
  expect_false(as.logical(tmp))
})
