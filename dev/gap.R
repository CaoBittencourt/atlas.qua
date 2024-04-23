# [FUNCTIONS] --------------------------------------------------------------
# - gap function ----------------------------------------------------------
fun_qua_gap <- function(x, y){

  # arguments validation
  stopifnot(
    "'x' must be a numeric." =
      is.numeric(x)
  )

  stopifnot(
    "'y' must be a numeric." =
      is.numeric(y)
  )

  # output
  return(max(x - y, 0))

}

