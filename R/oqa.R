# [FUNCTIONS] --------------------------------------------------------------
# - overqualification function (oqa) -------------------------------------------
oqa <- function(a_k, a_q, aeq_q, ub = 100){

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
    "'ub' must be numeric." =
      is.numeric(ub)
  )

  # output
  return(sqrt(
    sum(aeq_q * fun_qua_gap(a_k, a_q) ^ 2) /
      sum(aeq_q * fun_qua_gap(ub[[1]], a_q) ^ 2)
  ))

}

# - vectorized overqualification function ---------------------------------------------------------
fun_qua_oqa <- function(
    df_query_rows,
    df_data_rows,
    dbl_scale_ub = 100,
    chr_id_col = NULL
){

  # arguments validation
  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )

  # call data prep function
  call_list <- match.call()

  call_list[[1]] <- fun_qua_prep

  list_prep <- eval.parent(call_list)

  # calculate overqualification
  map(
    list_prep$a_k
    , ~ sqrt(
      rowSums(
        list_prep$aeq_q *
          fun_qua_gap(
            .x
            , list_prep$a_q
          ) ^ 2
      ) / rowSums(
        list_prep$aeq_q *
          fun_qua_gap(
            dbl_scale_ub[[1]]
            , list_prep$a_q
          ) ^ 2
      )
    )
  ) -> list_oqa

  # ouput
  return(list_oqa)

}

