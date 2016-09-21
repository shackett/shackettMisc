#' Reformat a tbl_df table into a matrix
#'
#' @param a_table a tbl_df object with column names \code{row_name}, \code{col_name}, and \code{value_name}
#' @param row_name column of \code{a_table} representing rows in the desired matrix
#' @param col_name column of \code{a_table} representing columns in the desired matrix
#' @param value_name column of \code{a_table} representing values of the matrix
#'
#' @return a matrix
#'
#' @seealso \code{\link{matrix_to_tbl_df}}
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' input_data <- dplyr::data_frame(a = rep(letters[1:3], each = 3),
#'                                 b = rep(LETTERS[1:3], times = 3),
#'                                 c = 1:9, extra_field = 2:10)
#' tbl_df_to_matrix(input_data, a, b, c)
#'
#' tbl_df_to_matrix_(input_data, "a", "b", "c")
#' @export
tbl_df_to_matrix <- function(a_table, row_name, col_name, value_name) {
  row_name <- as.character(substitute(row_name))
  col_name <- as.character(substitute(col_name))
  value_name <- as.character(substitute(value_name))
  tbl_df_to_matrix_(a_table, row_name, col_name, value_name)
}

#' @rdname tbl_df_to_matrix
#' @export
tbl_df_to_matrix_ <- function(a_table, row_name, col_name, value_name) {
  cast_formula <- as.formula(paste0(row_name, " ~ ", col_name))
  reshape2::acast(a_table, cast_formula, value.var = value_name)
}


#' Reformat a matrix int oa tbl_df table
#'
#' @param a_matrix a matrix
#' @param row_name name of output column where row names of \code{a_matrix} will be stored
#' @param col_name name of output column where column names of \code{a_matrix} will be stored
#' @param col_name name of output column where values from \code{a_matrix} will be stored
#'
#' @return a tbl_df object with columns matching the row names, column names and values of \code{a_matrix}
#'
#' @seealso \code{\link{tbl_df_to_matrix}}
#'
#' @importFrom dplyr %>%
#'
#' @examples
#'
#' input_matrix <- matrix(1:9, ncol = 3)
#' rownames(input_matrix) <- letters[1:3]
#' colnames(input_matrix) <- LETTERS[1:3]
#' matrix_to_tbl_df(input_matrix, "row", "col", "val")
#'
#' @export
matrix_to_tbl_df <- function(a_matrix, row_name, col_name, value_name){
  a_matrix %>%
    as.data.frame %>%
    dplyr::mutate(row = rownames(.)) %>%
    tidyr::gather("col", "value", -row) %>%
    dplyr::rename_(.dots = setNames(list("row", "col", "value"), c(row_name, col_name, value_name))) %>%
    tbl_df()
}
