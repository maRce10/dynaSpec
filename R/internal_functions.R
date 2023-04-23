# internal functions not to be called by users
# stop function that doesn't print call
stop2 <- function (...)
{
  stop(..., call. = FALSE)
}

# stop function that doesn't print call
warning2 <- function (...)
{
  warning(..., call. = FALSE)
}


