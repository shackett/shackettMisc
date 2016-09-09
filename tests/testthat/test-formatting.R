test_that("tbl_df_to_matrix converts tbl_df into expected matrix", {
  input_tbl_df <- data_frame(row = rep(letters[1:3], each = 3), col = rep(LETTERS[1:3], times = 3), val = 1:9, extra_field = 2:10)
  output_matrix <- tbl_df_to_matrix(input_tbl_df, "row", "col", "val")

  expect_is(output_matrix, "matrix")
  expect_equal(colnames(output_matrix), LETTERS[1:3])
  expect_equal(nrow(output_matrix), 3)
})

test_that("matrix_to_tbl_df converts matrix into expected tbl_df", {
  input_matrix <- matrix(1:9, ncol = 3)
  rownames(input_matrix) <- letters[1:3]
  colnames(input_matrix) <- LETTERS[1:3]
  output_tbl_df <- matrix_to_tbl_df(input_matrix, "row", "col", "val")

  expect_is(output_tbl_df, "tbl_df")
  expect_equal(colnames(output_tbl_df), c("row", "col", "val"))
  expect_equal(nrow(output_tbl_df), 9)
})
