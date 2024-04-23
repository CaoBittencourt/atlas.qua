# [FUNCTIONS] --------------------------------------------------------------
# - gap function ----------------------------------------------------------
fun_qua_gap <- function(x, y){

  # arguments validation
  stopifnot(
    "'x' must be numeric." =
      is.numeric(as.matrix(x))
  )

  stopifnot(
    "'y' must be numeric." =
      is.numeric(as.matrix(y))
  )

  x - y -> dbl_gap

  rm(x, y)

  dbl_gap[dbl_gap < 0] <- 0

  # output
  return(dbl_gap)

}

