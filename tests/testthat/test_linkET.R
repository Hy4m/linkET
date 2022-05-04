data("mtcars", package = "datasets")

## group id
id <- sample(LETTERS[1:3], 32, TRUE)

## matrix data
mt <- matrix_data(list(mtcars = mtcars))

## grouped matrix data
mt_grouped <- matrix_data(list(mtcars = mtcars), group = id)

## simple correlation matrix
corr <- correlate(mtcars)

## grouped correlation matrix
corr_grouped <- correlate(mtcars, group = id)


## matrix_data() can work
test_that("`matrix_data()` can work", {
  expect_s3_class(mt, "matrix_data")

  expect_s3_class(mt_grouped, "grouped_matrix_data")

  expect_s3_class(as_matrix_data(mtcars), "matrix_data")

  expect_s3_class(as_matrix_data(corr), "cor_matrix_data")

  expect_s3_class(as_matrix_data(corr_grouped), "grouped_matrix_data")
})

## as_md_tbl() can work
test_that("`as_md_tbl()` can works", {
  expect_s3_class(as_md_tbl(mt), "md_tbl")

  expect_s3_class(as_md_tbl(mt_grouped), "grouped_md_tbl")

  expect_s3_class(as_md_tbl(corr), "cor_md_tbl")

  expect_s3_class(as_md_tbl(corr_grouped), "cor_md_tbl")
})
