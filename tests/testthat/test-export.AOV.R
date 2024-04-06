library(testthat)

# Create res.AOV results from AOV.Grappe for testing


res.AOV <- structure(list(
  pvalue = data.frame(
    I_OD = c(0.0000, 0.4844, 0.0000),
    O_AGRUME = c(0, 1, 0)
  ),
  signif = data.frame(
    I_OD = c("***", "NS", "***"),
    O_AGRUME = c("***", "NS", "***")
  ),
  fvalue = data.frame(
    I_OD = c(32.931, 0.489, 9.288),
    O_AGRUME = c(108.683, 0.000, 8.681)
  ),
  lsd.result = list(
    Prod_Test1 = data.frame(
      I_OD = c(4.81, 5.83, 5.44, 4.17, 5.65, 5.62),
      Std_err = rep(0.26, 6),
      Grp = c("b", "a", "a", "c", "a", "a"),
      O_AGRUME = c(4.00, 3.12, 4.27, 3.64, 2.62, 4.36),
      Std_err = rep(0.36, 6),
      Grp = c("ab", "cd", "ab", "bc", "d", "a")),
    Prod_Test2 = data.frame(
      I_OD = c(4.80, 5.83, 5.44, 4.17, 5.65, 5.62),
      Std_err = rep(0.26, 6),
      Grp = c("b", "a", "a", "c", "a", "a"),
      O_AGRUME = c(4.00, 3.12, 4.27, 3.64, 2.62, 4.36),
      Std_err = rep(0.36, 6),
      Grp = c("ab", "cd", "ab", "bc", "d", "a"))

  )
), class = c("AOV.Grappe", "list"))


test_that("export.AOV correctly exports statistics and LSD results", {
  # Define temporary directory for testing
  temp_dir <- tempdir()



  # Define file names
  stat_file <- file.path(temp_dir)
  lsd_file <- file.path(temp_dir)

  # Run export.AOV function
  export.AOV(res.AOV, path=temp_dir, name = "res.AOV", print.dir = TRUE)

  # Check if statistics file is created
  expect_true(file.exists(stat_file))

  # Check if LSD file is created
  expect_true(file.exists(lsd_file))

  name.stat = "stat"
  name.LSD =  "LSD_test"

  # Read exported statistics file
  exported_stats <- read.csv2(file.path(stat_file, paste(Sys.Date(), "_", "res.AOV", "_", name.stat, ".csv", sep = "")),header = FALSE)

  # Check if exported statistics match original statistics
  expect_identical(exported_stats$V2[(4:5)], c("0,4844", "0"))
  expect_identical(exported_stats$V3[(7:8)], c("O_AGRUME", "108,683" ))

  # Read exported LSD file
  exported_lsd <- read.csv2(file.path(stat_file, paste(Sys.Date(), "_", "res.AOV", "_", name.LSD, ".csv", sep = "")),header = FALSE)


  # Check if exported LSD results match original LSD results
  expect_identical(exported_lsd$V5[(5:8)], c("4,27", "3,64", "2,62","4,36"))
  expect_identical(exported_lsd$V2[(13:16)], c("5,44" ,"4,17", "5,65", "5,62"))

  # Clean up temporary files
  unlink(stat_file)
  unlink(lsd_file)
})

test_that("export.AOV throws error for invalid inputs", {
  # Test with invalid res.from.AOV
  expect_error(export.AOV(123), "res.from.AOV  must be the result of the function AOV.Grappe")

  # Test with invalid save.stat
  expect_error(export.AOV(res.AOV, save.stat = "invalid"), "save.stat, save.LSD, and print.dir must be logical values.")

  # Test with invalid save.LSD
  expect_error(export.AOV(res.AOV, save.LSD = "invalid"), "save.stat, save.LSD, and print.dir must be logical values.")

  # Test with invalid print.dir
  expect_error(export.AOV(res.AOV, print.dir = "invalid"), "save.stat, save.LSD, and print.dir must be logical values.")

  # Test with invalid name
  expect_error(export.AOV(res.AOV, name = 123), "name, name.stat, and name.LSD must be character strings.")

  # Test with invalid name.stat
  expect_error(export.AOV(res.AOV, name.stat = 123), "name, name.stat, and name.LSD must be character strings.")

  # Test with invalid name.LSD
  expect_error(export.AOV(res.AOV, name.LSD = 123), "name, name.stat, and name.LSD must be character strings.")

  # Test with identical name.stat and name.LSD
  expect_error(export.AOV(res.AOV, name.stat = "stat", name.LSD = "stat"), "name.stat and name.LSD must be different")

  # Test with invalid path
  expect_error(export.AOV(res.AOV, path = "test_file"), "folder does not exist, please indicate an existing path")
  })

