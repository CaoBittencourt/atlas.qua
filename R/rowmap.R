# [FUNCTIONS] -------------------------------------------------------------
# - rowmap function -------------------------------------------------------
rowmap <- function(df_data, fun_function, ...){

  # arguments validation
  stopifnot(
    "'df_data' must be a data frame." =
      is.data.frame(df_data)
  )

  stopifnot(
    "'fun_function' must be a function." =
      is.function(fun_function)
  )

  # rowmap function
  df_data %>%
    t() %>%
    as_tibble() %>%
    map(fun_function, ...) %>%
    bind_rows() %>%
    t() %>%
    as_tibble() %>%
    set_names(names(
      df_data
    )) -> df_data

  # output
  return(df_data)

}
