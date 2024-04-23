# [FUNCTIONS] --------------------------------------------------------------
# - sufficient qualification function (sqa) ---------------------------------
sqa <- function(a_k, a_q, aeq_q){return(1 - uqa(a_k, a_q, aeq_q))}

# - vectorized sufficient qualification function ---------------------------------------------------------
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

  # calculate sufficient qualification
  map(
    list_uqa
    , ~ 1 - .x
  ) -> list_sqa

  rm(list_uqa)

  # ouput
  return(list_sqa)

}
