# internal function not to be called by users
# stop function that doesn't print call
stop2 <- function (...)
{
  stop2(..., call. = FALSE)
}