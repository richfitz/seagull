

# seagull

[![Linux Build Status](https://travis-ci.org/richfitz/seagull.svg?branch=master)](https://travis-ci.org/richfitz/seagull)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/richfitz/seagull?svg=true)](https://ci.appveyor.com/project/richfitz/seagull)

Portable file locking for R.  The approach and interface is based on the "inter process" locks in the Python [fasteners](https://pypi.python.org/pypi/fasteners) package.

The package provides a function `with_flock` which evaluates an expression only if a file lock can be obtained.  By default it will block and periodically retry to  open the file.

For example, suppose you want to place an advisory lock in a directory `path`.  We can create a lockfile in that directory and then we can do whatever we want in the directory -- other processes obeying this convention will wait:


```r
path <- tempfile()
dir.create(path)
lockfile <- file.path(path, "lock")
realfile <- file.path(path, "db")
seagull::with_flock(lockfile, {
  prev <- if (file.exists(realfile)) readLines(realfile) else character(0)
  writeLines(c(prev, paste(Sys.getpid(), "wuz here")), realfile)
})
```

```
## NULL
```


```r
readLines(realfile)
```

```
## [1] "16179 wuz here"
```

The code above will wait until the db file is ready, read it, add a new line to it, then release the lock.  If multiple processes were trying to do this at once they would access the file in an unspecified order but a race condition between read and write is eliminated.

For example:


```r
cl <- parallel::makeCluster(4, "PSOCK")
ign <- parallel::clusterEvalQ(cl, library(seagull))
pids <- unlist(parallel::clusterCall(cl, Sys.getpid))
pids
```

```
## [1] 16188 16197 16206 16215
```

Run the code from above (slightly awkward due to controlling the cluster):


```r
f <- function(lockfile, realfile) {
  seagull::with_flock(lockfile, {
    prev <- if (file.exists(realfile)) readLines(realfile) else character(0)
    writeLines(c(prev, paste(Sys.getpid(), "wuz here")), realfile)
  })
}
ign <- parallel::clusterCall(cl, f, lockfile, realfile)
```

The file now contains four entries, one from each node (plus the original line from the time we ran it locally):


```r
writeLines(readLines(realfile))
```

```
## 16179 wuz here
## 16206 wuz here
## 16215 wuz here
## 16197 wuz here
## 16188 wuz here
```

Note also that the order of the lines written is not the same as the order of the PIDs.  Because the file is polled for access by each process it is undefined which order they will get access in.



## Design condsiderations

**Portability**.  On unix systems this uses `fcntl` which should work on Linux and BSD based systems, modern NFS and SMB.  On Windows it uses the Win32 API (`_locking` from `sys/locking.h`).

**This package does not use R's connection objects**.  A sane native implementation of file locking would use the result of `file(...)`, but because of the way R's connection objects are implemented this is not possible (the actual file descriptor object is stored in the private data of the connection object and the format of that is non-API).

Unfortunately with the [implementation of unix file locking](http://0pointer.de/blog/projects/locking.html) the lock will be broken if _any_ connection to the object is closed.  So it is in general unsafe to open a second connection to the file object.  A possibly safe pattern would be:

1. Acquire lock
2. Open a second connection to the file
3. Write whatever you need to write
4. Close the file
5. Immediately close the lock

The lock will be lost at step 4 above but that should not matter as `seagull` never writes to the files it holds.  Note that this situation applies to things like `writeLines`, `write.csv`, etc; those functions encompass steps 2-4 in the above.

Future versions may implement the full connection interface to allow passing the locked file around as an R connection.

## Installation


```r
devtools::install_github("richfitz/seagull")
```

## License

MIT + file LICENSE © [Rich FitzJohn](https://github.com/richfitz).
