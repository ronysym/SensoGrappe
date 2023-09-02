#' STAR.Grappe : function for encode pvalue to stars
#'
#'
#' @param values dataframe, vector or column of a matrix with pvalue to be coded into stars
#'
#' @examples
#'attach(iris)
#'STAR.Grappe(iris$Sepal.Width)
#'
#'
#' @export

STAR.Grappe <- function(values)
{
  pval <- c(0.001, 0.01, 0.05)
  star <- c("***", "**", "*", "NS")

  res.star <- vector()
  for(v in 1:length(values))
  {
    if(is.na(values[v]))
    {
      res.star[v] <- "-"
    } else {
      verif <- grep(TRUE, values[v] < pval)
      if(length(verif) == 0)
      {
        res.star[v] <- "NS"
      } else {
        res.star[v] <- star[verif[1]]
      }
    }
  }
  return(res.star)
}
