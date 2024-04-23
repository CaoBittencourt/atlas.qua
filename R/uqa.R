# [FUNCTIONS] --------------------------------------------------------------
# - underqualification function (uqa) -------------------------------------------
uqa <- function(a_k, a_q, aeq_q, lb = 0){

  # arguments validation
  stopifnot(
    "'a_k' must be a numeric vector." =
      is.numeric(a_k)
  )

  stopifnot(
    "'a_q' must be a numeric vector the same length as 'a_k'." =
      all(
        is.numeric(a_q)
        , length(a_q) ==
          length(a_k)
      )
  )

  stopifnot(
    "'aeq_q' must be a numeric vector the same length as 'a_q'." =
      all(
        is.numeric(aeq_q)
        , length(aeq_q) ==
          length(a_q)
      )
  )

  stopifnot(
    "'lb' must be numeric." =
      is.numeric(lb)
  )

  # output
  return(sqrt(
    sum(aeq_q * fun_qua_gap(a_q, a_k) ^ 2) /
      sum(aeq_q * fun_qua_gap(a_q, lb[[1]]) ^ 2)
  ))

}

# - vectorized underqualification function ---------------------------------------------------------
fun_qua_uqa <- function(
    df_query_rows,
    df_data_rows,
    dbl_scale_lb = 0,
    chr_id_col = NULL
){

  # arguments validation
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )

  # call data prep function
  call_list <- match.call()

  call_list[[1]] <- fun_qua_prep

  list_prep <- eval.parent(call_list)

  # calculate underqualification
  map(
    list_prep$a_k
    , ~ sqrt(
      rowSums(
        list_prep$aeq_q *
          fun_qua_gap(
            list_prep$a_q
            , .x
          ) ^ 2
      ) / rowSums(
        list_prep$aeq_q *
          fun_qua_gap(
            list_prep$a_q
            , dbl_scale_lb[[1]]
          ) ^ 2
      )
    )
  ) -> list_uqa

  # set names
  map(
    list_uqa
    , set_names
    , list_prep$
      names
  ) -> list_uqa

  # ouput
  return(list_uqa)

}

