start_cluster <- function(n, ...) {
  Sys.setenv(R_TESTS="")
  cl <- parallel::makeCluster(n, "PSOCK", ...)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterEvalQ(cl, {
    loadNamespace("seagull")
    attach(getNamespace("seagull"), name=paste0("package:seagull"))
  })
  on.exit()
  cl
}
stop_cluster <- function(cl) {
  parallel::stopCluster(cl)
}

with_cluster <- function(n, expr) {
  cl <- start_cluster(n)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterEvalQ(cl, expr)
}

## Used throughout test-seagull
f_remote <- function(filename, what) {
  fh <- seagull_open(filename)
  on.exit(seagull_close(fh))
  switch(what,
         open=fcntl_lock(fh, TRUE),
         close=fcntl_lock(fh, FALSE),
         state=fcntl_state(fh))
}

## See ${RSRC}/src/library/parallel/R/detectCores.R
cores <- function() {
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(chk) && (chk != "false")) 2L else 4L
}
