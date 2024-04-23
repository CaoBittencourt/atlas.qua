# [FUNCTIONS] -------------------------------------------------------------
# - data prep helper function ---------------------------------------------
fun_qua_prep <- function(
    df_query_rows,
    df_data_rows,
    chr_id_col = NULL,
    ...
){

  # arguments validation
  stopifnot(
    "'df_query_rows' must be a data frame." =
      is.data.frame(df_query_rows)
  )

  stopifnot(
    "'df_data_rows' must be a data frame." =
      is.data.frame(df_data_rows)
  )

  stopifnot(
    "'chr_id_col' must be either NULL or a character string." =
      any(
        is.null(chr_id_col)
        , is.character(chr_id_col)
      )
  )

  # data wrangling
  if(length(chr_id_col)){

    chr_id_col[[1]] -> chr_id_col

    df_query_rows %>%
      pull(!!sym(
        chr_id_col
      )) -> chr_id_query

    df_data_rows %>%
      pull(!!sym(
        chr_id_col
      )) -> chr_id_data

  } else {

    paste0(
      'query',
      1:nrow(df_query_rows)
    ) -> chr_id_query

    paste0(
      'data',
      1:nrow(df_data_rows)
    ) -> chr_id_data

  }

  rm(chr_id_col)

  Filter(
    function(x){all(is.numeric(x))}
    , df_query_rows
  ) -> df_query_rows

  df_data_rows[names(
    df_query_rows
  )] -> df_data_rows

  df_query_rows %>%
    split(chr_id_query) ->
    list_query

  map(
    list_query
    , ~ .x[rep(
      1, nrow(df_data_rows))
      , ]
  ) -> list_query

  # rowmap attribute equivalence function
  rowmap(
    df_data_rows
    , fun_aeq_aequivalence
  ) -> df_aeq

  # output
  return(list(
    a_k = list_query,
    a_q = df_data_rows,
    aeq_q = df_aeq,
    names = chr_id_data
  ))

}

