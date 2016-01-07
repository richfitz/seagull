context("flock")

## For debugging purposes, this might be useful:
## In an R session run:
##   cl <- start_cluster(1, outfile="", manual=TRUE, port=11242)
## In a terminal window, run:
##   R -d gdb --args MASTER=localhost PORT=11242 OUT= TIMEOUT=2592000 METHODS=TRUE XDR=TRUE
## Start R (run) then run
##   parallel:::.slaveRSOCK()
## to set up the main loop.  This allows use of both R and C level
## debugging, plus no output redirection.

test_that("simple (fcntl)", {
  s <- flock(tempfile())

  expect_false(s$acquired)
  expect_true(s$acquire(timeout=0.1))
  expect_true(s$acquired)
  expect_true(file.exists(s$filename))

  s$release()
  expect_false(s$acquired)
  expect_error(s$release(), "Cannot release a lock")
  expect_true(file.exists(s$filename)) # NOTE: not deleted.
})

test_that("simple (hack)", {
  s <- flock(tempfile(), "hack")

  expect_false(s$acquired)
  expect_true(s$acquire(timeout=0.1))
  expect_true(s$acquired)
  expect_true(file.exists(s$filename))

  s$release()
  expect_false(s$acquired)
  expect_error(s$release(), "Cannot release a lock")
  expect_false(file.exists(s$filename)) # NOTE: is deleted
})

test_that("lock_state", {
  skip_on_os("windows")
  fn <- tempfile()
  cl <- start_cluster(1)
  on.exit(stop_cluster(cl))
  pid <- parallel::clusterCall(cl, Sys.getpid)[[1]]

  s <- flock(fn)
  expect_true(s$acquire(timeout=0.1))
  expect_equal(parallel::clusterCall(cl, f_remote, fn, "state")[[1]],
               list(locked=TRUE, pid=Sys.getpid()))

  expect_true(s$release())
  expect_equal(parallel::clusterCall(cl, f_remote, fn, "state")[[1]],
               list(locked=FALSE, pid=pid))
})

test_that("parallel", {
  cl <- start_cluster(2)
  on.exit(stop_cluster(cl))

  ## This works ok
  pids <- unlist(parallel::clusterCall(cl, Sys.getpid))

  fn <- tempfile()
  s <- flock(fn)
  s$acquire(timeout=0.1)

  g_remote <- function(filename) {
    s <- flock(filename)
    on.exit(seagull_close(s$handle))
    s$acquire(timeout=0.1, error=FALSE)
  }
  res <- parallel::clusterCall(cl, g_remote, fn)

  expect_equal(vapply(res, function(x) x[[1]], logical(1)),
               c(FALSE, FALSE))
  expect_is(res[[1]][[2]], "LockFailed")
  expect_is(res[[2]][[2]], "LockFailed")

  if (!is_windows()) {
    expect_equal(parallel::clusterCall(cl, f_remote, fn, "state")[[1]],
                 list(locked=TRUE, pid=Sys.getpid()))
  }
})

test_that("multi write (flock)", {
  f <- function(lock, write) {
    str <- paste(Sys.info()[["nodename"]], Sys.getpid(), rand_str(10),
                 collapse="")
    s <- flock(lock)
    ok <- s$acquire(timeout=1, error=FALSE)
    if (ok[[1]]) {
      if (file.exists(write)) {
        to_write <- c(readLines(write), str)
      } else {
        to_write <- str
      }
      writeLines(to_write, write)
      s$release()
    }
    list(ok=ok[[1]], str=str)
  }

  fl <- tempfile()
  fd <- tempfile()

  cl <- start_cluster(cores())
  on.exit({
    stop_cluster(cl)
    file_remove(fl)
    file_remove(fd)
  })

  pids <- unlist(parallel::clusterCall(cl, Sys.getpid))
  res_cl <- parallel::clusterCall(cl, f, fl, fd)

  res <- readLines(fd)
  expect_equal(length(res), length(cl))

  expect_equal(vapply(res_cl, function(x) x[[1L]], logical(1)),
               rep(TRUE, length(cl)))
  dat <- vapply(res_cl, function(x) x[[2L]], character(1))

  expect_true(all(dat %in% res))
  expect_true(all(pids %in% as.integer(sub(".* ([0-9]+) .*", "\\1", dat))))
})

test_that("multi write (with_flock)", {
  f <- function(lock, write) {
    str <- paste(Sys.info()[["nodename"]], Sys.getpid(), rand_str(10),
                 collapse="")
    with_flock(lock, timeout=0.1, error=FALSE, {
      x <- if (file.exists(write)) c(readLines(write), str) else str
      writeLines(x, write)
      str
    })
  }

  fl <- tempfile()
  fd <- tempfile()

  cl <- start_cluster(cores())
  on.exit({
    stop_cluster(cl)
    file_remove(fl)
    file_remove(fd)
  })

  pids <- unlist(parallel::clusterCall(cl, Sys.getpid))
  res_cl <- parallel::clusterCall(cl, f, fl, fd)

  res <- readLines(fd)
  expect_equal(length(res), length(cl))

  expect_equal(vapply(res_cl, function(x) x[[1L]], logical(1)),
               rep(TRUE, length(cl)))
  dat <- vapply(res_cl, function(x) x[[2L]], character(1))

  expect_true(all(dat %in% res))
  expect_true(all(pids %in% as.integer(sub(".* ([0-9]+) .*", "\\1", dat))))
})
