% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export.AOV.R
\name{export.AOV}
\alias{export.AOV}
\title{export.AOV : Export results from AOV.Grappe}
\usage{
export.AOV(
  res.from.AOV,
  path = tempdir(),
  save.stat = TRUE,
  save.LSD = TRUE,
  name = NULL,
  name.stat = "stat",
  name.LSD = "LSD_test",
  print.dir = TRUE
)
}
\arguments{
\item{res.from.AOV}{list results from \code{\link[=AOV.Grappe]{AOV.Grappe()}}}

\item{path}{path where file will be saved : if path="." files will save in the working directory}

\item{save.stat}{logical save pvalue, signif, F, DF et models results. TRUE by default}

\item{save.LSD}{logical save LS means results. TRUE by default TRUE by default}

\item{name}{character name of the file}

\item{name.stat}{character name for the Stat Files (defaut is "stat")}

\item{name.LSD}{character name for the LSD Files (defaut is "LSD_stat")}

\item{print.dir}{logical print the directory name. TRUE by default}
}
\description{
Export results from \code{\link[=AOV.Grappe]{AOV.Grappe()}}
}
\examples{
data(wine)
Var.Grappe(wine)
winef <- Var.Grappe(wine, column = c(1:2), type = "factor")
res.AOV <- AOV.Grappe(winef, column = c(3:4), "ProductName + (1|CJ)+ (1|ProductName:CJ)")
\dontrun{
export.AOV(res.AOV)
}
mycolor <- c("#EDF8E9", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32")
graph.AOV(res.AOV, color.graph = mycolor, y.label = "Moyenne")
graph.AOV(res.AOV, title.graph = "Sensory Score", add.moy = TRUE, x.ordered = 1, ymin = 1, ymax = 9)

}
\seealso{
\code{\link[=AOV.Grappe]{AOV.Grappe()}}
}
