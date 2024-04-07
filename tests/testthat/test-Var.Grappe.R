library(testthat)

# Create a sample dataframe
df <- data.frame(
  col1 = c("A", "B", "C"),
  col2 = c(1, 2, 3),
  bol3 = c(4.1, 5.2, 6.3),
  stringsAsFactors = FALSE
)



# Tests for successful cases
test_that("Var.Grappe works correctly", {
  # Test to check if the function correctly changes the column type to factor
  df_factor <- Var.Grappe(x = df, column = 1, type = "factor")
  expect_equal(class(df_factor[, 1]), "factor")

  # Test to check if the function correctly changes the column type to numeric
  df_numeric <- Var.Grappe(x = df, column = 2, type = "numeric")
  expect_equal(class(df_numeric[, 2]), "numeric")

  # Test to check if the function correctly changes the column type to integer
  df_integer <- Var.Grappe(x = df, column = 3, type = "integer")
  expect_equal(class(df_integer[, 3]), "integer")

  # Test with vector of column numbers
  df_vector_col <- Var.Grappe(df, column = 2:3, type = "factor")
  expect_equal(class(df_vector_col[, 2]), "factor")
  expect_equal(class(df_vector_col[, 3]), "factor")

  # Test with single column name
  df_single_col <- Var.Grappe(df, column = "col2", type = "factor")
  expect_equal(class(df_single_col[, "col2"]), "factor")

  # Test with vector of column names
  df_vector_col <- Var.Grappe(df, column = c("col2", "bol3"), type = "factor")
  expect_equal(class(df_vector_col[, "col2"]), "factor")
  expect_equal(class(df_vector_col[, "bol3"]), "factor")

})

# Tests for error cases
test_that("Var.Grappe handles errors correctly", {
  # Test to check if the function throws an error when x is not a dataframe
  expect_error(Var.Grappe(x = "not a dataframe"),
               "Argument 'x' must be a dataframe.")

  # Test to check if the function throws an error when column is not numeric
  expect_error(Var.Grappe(x = df, column = list()),
               "Argument 'column' must be either character or numeric.")

  # Test to check if the function throws an error when type is not one of the allowed types
  expect_error(Var.Grappe(x = df, column = 1, type = "not a valid type"),
               "Argument 'type' must be one of 'factor', 'numeric', or 'integer'.")

  # Test to check if the function throws an error when verbose is not logical
  expect_error(Var.Grappe(x = df, verbose = "not logical"),
               "Argument 'verbose' must be logical.")

  # Test to check if the function throws an error when filter is neither numeric nor a character
  expect_error(Var.Grappe(x = df, filter = list()),
               "Argument 'filter' must be either character or numeric.")

  # Test with invalid column number
  expect_error(Var.Grappe(df, column = 5, type = "factor"),
               "Argument 'column' must be within the range of the number of columns in the dataframe.")

   # Test with invalid column name
  expect_error(Var.Grappe(df, column = "invalid_column_name", type = "factor"),
               "Argument 'column' must contain valid column names from the dataframe.")

  # Test with vector of column names and one invalid column name
  expect_error(Var.Grappe(df, column = c("col2","invalid_column_name"), type = "factor"),
               "Argument 'column' must contain valid column names from the dataframe.")

})


test_that("Var.Grappe prints correct output", {
  # Capture the output of the function
  captured_output <- gsub(" ", "", capture.output(Var.Grappe(df)))

  # Define the expected output
  expected_output <-
"     Column Type of variable
col1 1      character
col2 2      numeric
bol3 3      numeric         "
  expected_output_without<-gsub(" ", "",expected_output)

  # Compare the captured output with the expected output
  expect_equal(paste(captured_output, collapse = "\n"), expected_output_without)
})


test_that("Var.Grappe prints correct output", {
  # Capture the output of the function
  captured_output <- gsub(" ", "", capture.output(Var.Grappe(df, filter="col")))

  # Define the expected output
  expected_output <- "     Column Type of variable
col1 1      character
col2 2      numeric         "
  expected_output_without<-gsub(" ", "",expected_output)


  # Compare the captured output with the expected output
  expect_equal(paste(captured_output, collapse = "\n"), expected_output_without)
})
