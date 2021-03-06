% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formatting.R
\name{tbl_df_to_matrix}
\alias{tbl_df_to_matrix}
\alias{tbl_df_to_matrix_}
\title{Reformat a tbl_df table into a matrix}
\usage{
tbl_df_to_matrix(a_table, row_name, col_name, value_name)

tbl_df_to_matrix_(a_table, row_name, col_name, value_name)
}
\arguments{
\item{a_table}{a tbl_df object with column names \code{row_name}, \code{col_name}, and \code{value_name}}

\item{row_name}{column of \code{a_table} representing rows in the desired matrix}

\item{col_name}{column of \code{a_table} representing columns in the desired matrix}

\item{value_name}{column of \code{a_table} representing values of the matrix}
}
\value{
a matrix
}
\description{
Reformat a tbl_df table into a matrix
}
\examples{

input_data <- dplyr::data_frame(a = rep(letters[1:3], each = 3),
                                b = rep(LETTERS[1:3], times = 3),
                                c = 1:9, extra_field = 2:10)
tbl_df_to_matrix(input_data, a, b, c)

tbl_df_to_matrix_(input_data, "a", "b", "c")
}
\seealso{
\code{\link{matrix_to_tbl_df}}
}

