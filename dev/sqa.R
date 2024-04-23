# [FUNCTIONS] --------------------------------------------------------------
# - sufficient qualification function (sqa) ---------------------------------------------------------
fun_qua_sqa <- function(
    df_query_rows,
    df_data_rows,
    chr_id_col = NULL
){

  # call uqa function
  call_list <- match.call()

  call_list[[1]] <- fun_qua_uqa

  list_uqa <- eval.parent(call_list)

  rm(call_list)

  return(list_uqa)

  # calculate sufficient qualification
  # map -> 1 - uqa -> list_uqa

  # ouput
  return(list_sqa)

}
