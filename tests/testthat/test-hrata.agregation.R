library(testthat)




# Tests for successful cases
test_that("hrata.agregation works correctly", {
  # Load example data
  data(rose)
  data(rose.attribute)

  # Test 1: Check if the function returns an object of class 'hrata' and 'list'
  res <- hrata.agregation(data = rose, h.table = rose.attribute, crit.agreg = "max")
  expect_true(inherits(res, "hrata"))
  expect_true(inherits(res, "list"))

  # Test 2: Check if the output list has the correct length
  expect_length(res, 3)

  # Test 3: Check if the output list contains the correct elements
  expect_equal(names(res), c("hierarchical.data", "lfam.name", "lcat.name"))

  # Test 4: Check if 'hierarchical.data' is a data frame
  expect_equal(class(res$hierarchical.data), "data.frame")

  # Test 5: Check if 'lfam.name' and 'lcat.name' are lists
  expect_type(res$lfam.name, "list")
  expect_type(res$lcat.name, "list")

  # Test 6: Check if 'data' has the correct column names in the first and second columns
  allowed_judge_cols <- c("CJ", "Judge", "Juge")
  allowed_product_cols <- c("Product", "Produit")

  expect_true(colnames(rose)[1] %in% allowed_judge_cols)
  expect_true(colnames(rose)[2] %in% allowed_product_cols)

  # Test 7: Check if 'hierarchical.data' has the correct column names (considering allowed_judge_cols and allowed_product_cols)
  expect_true(colnames(res$hierarchical.data)[1] %in% allowed_judge_cols)
  expect_true(colnames(res$hierarchical.data)[2] %in% allowed_product_cols)


  # Test 7: Check if 'lfam.name' and 'lcat.name' have the correct lengths
  expect_length(res$lfam.name, length(unique(rose.attribute$Famille)))
  expect_length(res$lcat.name, length(unique(rose.attribute$Categorie)))


  # Test : Check values in lfam.name,lcat.name, hierarchical.data
  expect_equal(res$lfam.name[[1]], c( "C_Agrumes" ,"C_Fruits_rouges","C_Fruits_blancs" , "C_Fruits_jaunes", "C_Fruits_exotique"))
  expect_equal(res$lcat.name[[7]], c("A_Jacinthe" ,"A_Lilas"  ,  "A_Muguet"  , "A_Narcisse"))
  expect_equal(res$hierarchical.data[[3]][1:8],c(0, 2, 3 ,0 ,3, 0, 1, 2 ))


})



# Tests for error conditions
test_that("hrata.agregation error tests", {
  # Load example data
  data(rose)
  data(rose.attribute)

  # Test with 'data' not being a data frame
  bad_data <- list()
  expect_error(hrata.agregation(data = bad_data, h.table = rose.attribute, crit.agreg = "max"),
               "The 'data' argument must be a data frame.")

  # Test with 'h.table' not being a data frame
  bad_h_table <- list()
  expect_error(hrata.agregation(data = rose, h.table = bad_h_table, crit.agreg = "max"),
               "The 'h.table' argument must be a data frame.")

  # Test with an invalid 'crit.agreg' value
  expect_error(hrata.agregation(data = rose, h.table = rose.attribute, crit.agreg = "median"),
               "The 'crit.agreg' argument must be either 'max' or 'mean'.")

  # Test with 'data' having less than four columns
  bad_data <- rose[, c(1:3)]
  expect_error(hrata.agregation(data = bad_data, h.table = rose.attribute, crit.agreg = "max"),
               "The 'data' argument must contain at least four columns: 'Judge' and 'Product' and 2 attribute")

  # Test with 'h.table' having more or less than three columns
  bad_h_table <- rose.attribute[, c(1:2)]
  expect_error(hrata.agregation(data = rose, h.table = bad_h_table, crit.agreg = "max"),
               "The 'h.table' argument must contain three columns: 'Family', 'Category', and 'Attribute'.")

  # Test with incorrect column names in 'data'
  bad_data <- rose
  colnames(bad_data)[1] <- "Invalid_Judge"
  expect_error(hrata.agregation(data = bad_data, h.table = rose.attribute, crit.agreg = "max"),
               "The first column of the 'data' argument must be named 'CJ', 'Judge', or 'Juge', and the second column must be named 'Product' or 'Produit'.")

  # Test with incorrect column names in 'h.table'
  bad_h_table <- rose.attribute
  colnames(bad_h_table)[1] <- "Invalid_Family"
  expect_error(hrata.agregation(data = rose, h.table = bad_h_table, crit.agreg = "max"),
               "The 'h.table' column names must be either 'Family', 'Category', 'Attribute' or 'Famille', 'Categorie', 'Attribut'")


  # Test with incorrect category in in 'h.table'
  bad_h_table <- rose.attribute
  bad_h_table$Categorie[31] <- "C_Fleur_Not_Good"
  expect_error(hrata.agregation(data = rose, h.table = bad_h_table, crit.agreg = "max"),
               "The 'h.table' and 'data' don't match for Familly or Category")

  # Test with incorrect family in in 'h.table'
  bad_h_table <- rose.attribute
  bad_h_table$Famille[31] <- "F_Fleur_Not_Good"
  expect_error(hrata.agregation(data = rose, h.table = bad_h_table, crit.agreg = "max"),
               "The 'h.table' and 'data' don't match for Familly or Category")


  # Test with incorrect attributes in in 'h.table'
  bad_h_table <- rose.attribute
  bad_h_table$Attribut[31] <- "A_Fleur_Not_Good"
  expect_error(hrata.agregation(data = rose, h.table = bad_h_table, crit.agreg = "max"),
               "The 'h.table' and 'data' don't match, please verify")



  # Test with incorrect Family in in 'data'
  bad_data <- rose
  names(bad_data)[3]<-"F_Fruit_Bad"
  expect_error(hrata.agregation(data = bad_data, h.table = rose.attribute, crit.agreg = "max"),
               "The 'h.table' and 'data' don't match, please verify")


  # Test with incorrect Attribute in in 'data'
  bad_data <- rose
  names(bad_data)[5]<-"A_Fruit_Bad"
  expect_error(hrata.agregation(data = bad_data, h.table = rose.attribute, crit.agreg = "max"),
               "The 'h.table' and 'data' don't match, please verify")

      })
