# [FUNCTIONS] --------------------------------------------------------------
# - underqualification function (uqa) ---------------------------------------------------------
fun_qua_uqa <- function(
    df_query_rows,
    df_data_rows,
    chr_id_col = NULL
){

  # call data prep function
  call_list <- match.call()

  call_list[[1]] <- fun_qua_prep

  list_prep <- eval.parent(call_list)

  rm(call_list)

  return(list_prep)

  # calculate underqualification

  # ouput
  return(list_uqa)

}