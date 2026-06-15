# Suppress R CMD check notes for column names used in dplyr/ggplot2 NSE
utils::globalVariables(c(
  "dimension", "concept", "descripteur",
  "subject", "product",
  "y", "group"
))
