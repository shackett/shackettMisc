#' Reformat a tbl_df table into a matrix
#'
#' @param a_table a tbl_df object with column names \code{row_name}, \code{col_name}, and \code{value_name}
#' @param row_name column of \code{a_table} representing rows in the desired matrix
#' @param col_name column of \code{a_table} representing columns in the desired matrix
#' @param value_name column of \code{a_table} representing values of the matrix
#' @return a matrix
#' @seealso \code{\link{matrix_to_tbl_df}}
#' @examples input_data <- data_frame(row = rep(letters[1:3], each = 3), col = rep(LETTERS[1:3], times = 3), val = 1:9, extra_field = 2:10)
#' tbl_df_to_matrix(input_data, "row", "col", "val")
#' @export
tbl_df_to_matrix <- function(a_table, row_name, col_name, value_name){

  requireNamespace("dplyr")
  requireNamespace("tidyr")

  spread_table <- a_table %>% select_(.dots = c(row_name, col_name, value_name)) %>%
    spread_(col_name, value_name)
  rownames(spread_table) <- spread_table %>% select_(row_name) %>% unlist()
  spread_table %>%
    select_(.dots = as.list(setdiff(colnames(spread_table), row_name))) %>%
    as.matrix

}

#' Reformat a matrix int oa tbl_df table
#'
#' @param a_matrix a matrix
#' @param row_name name of output column where row names of \code{a_matrix} will be stored
#' @param col_name name of output column where column names of \code{a_matrix} will be stored
#' @param col_name name of output column where values from \code{a_matrix} will be stored
#' @return a tbl_df object with columns matching the row names, column names and values of \code{a_matrix}
#' @seealso \code{\link{tbl_df_to_matrix}}
#' @examples input_matrix <- matrix(1:9, ncol = 3)
#' rownames(input_matrix) <- letters[1:3]
#' colnames(input_matrix) <- LETTERS[1:3]
#' matrix_to_tbl_df(input_matrix, "row", "col", "val")
#' @export
matrix_to_tbl_df <- function(a_matrix, row_name, col_name, value_name){

  requireNamespace("dplyr")
  requireNamespace("tidyr")

  a_matrix %>% as.data.frame %>% mutate(row = rownames(.)) %>%
    gather("col", "value", -row) %>%
    rename_(.dots = setNames(list("row", "col", "value"), c(row_name, col_name, value_name))) %>%
    tbl_df

}

