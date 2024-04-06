#' export.AOV : Export results from AOV.Grappe
#'
#' @description Export results from [AOV.Grappe()]
#'
#' @param res.from.AOV   list results from [AOV.Grappe()]
#' @param path   path where file will be saved : if path="." files will save in the working directory
#' @param	save.stat logical save pvalue, signif, F, DF et models results. TRUE by default
#' @param	save.LSD  logical save LS means results. TRUE by default TRUE by default
#' @param	name      character name of the file
#' @param	name.stat character name for the Stat Files (defaut is "stat")
#' @param	name.LSD  character name for the LSD Files (defaut is "LSD_stat")
#' @param	print.dir logical print the directory name. TRUE by default
#'
#' @import utils
#'
#' @seealso [AOV.Grappe()]
#'
#'
#' @examples
#' data(wine)
#' Var.Grappe(wine)
#' winef <- Var.Grappe(wine, column = c(1:2), type = "factor")
#' res.AOV <- AOV.Grappe(winef, column = c(3:4), "ProductName + (1|CJ)+ (1|ProductName:CJ)")
#' \dontrun{
#' export.AOV(res.AOV)
#' }
#' mycolor <- c("#EDF8E9", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32")
#' graph.AOV(res.AOV, color.graph = mycolor, y.label = "Moyenne")
#' graph.AOV(res.AOV, title.graph = "Sensory Score", add.moy = TRUE, x.ordered = 1, ymin = 1, ymax = 9)
#'
#' @export
export.AOV <- function(res.from.AOV, path=tempdir() ,save.stat = TRUE, save.LSD = TRUE,  name = NULL, name.stat = "stat", name.LSD = "LSD_test", print.dir = TRUE) {

  # Check if res.from.AOV come from AOV.Grappe
  if (!any(class(res.from.AOV)=="AOV.Grappe")){
    stop("res.from.AOV  must be the result of the function AOV.Grappe", call. = FALSE)
  }

  # Check if save.stat, save.LSD and print.dir are logical
  if (!is.logical(save.stat) || !is.logical(save.LSD) || !is.logical(print.dir)) {
    stop("save.stat, save.LSD, and print.dir must be logical values.")
  }

  if(is.null(name)){
    name=as.character(substitute(res.from.AOV))
  }

  # Check if name, name.stat, and name.LSD are characters
  if (!is.character(name) || !is.character(name.stat) || !is.character(name.LSD)) {
    stop("name, name.stat, and name.LSD must be character strings.")
  }

  # Check that the file names are different
  if (save.stat == TRUE && save.LSD == TRUE && name.stat == name.LSD) {
    stop("name.stat and name.LSD must be different")
  }

  # Check that the folder names exists
  if (isFALSE(dir.exists(path))) {
    stop("folder does not exist, please indicate an existing path")
  }


  # Enregistre les stats associees a l'ANOVA
  if (save.stat) {
    # Ecrit les sorties dans un fchier .csv


    sink(file =  file.path(path, paste(Sys.Date(), "_", name, "_", name.stat, ".csv", sep = "")))
    # Boucle pour ecrire les resultats dans le fichier de sortie
    for (i in c(1, 3))
    {
      # Nom de la statistique
      cat(paste("\n", names(res.from.AOV[i]), "\n", sep = ""))
      # Tableau associe
      cat(write.csv2(x = res.from.AOV[[i]]))
    }
    # Ferme le fichier de sortie
    sink(file = NULL)
    # Affiche le chemin du fichier enregistre
    if (print.dir == TRUE) {
      cat("\nStats were recorded :\n", normalizePath(path), "\n")
    }
  }

  # Idem avec les LSD
  if (save.LSD) {
    sink(file = file.path(path, paste(Sys.Date(), "_", name, "_", name.LSD, ".csv", sep = "")))
    # Extrait les LSD dans un objet temporaire car les LSD constituent une sous liste
    # (-> Remonte les LSD d'un niveau)
    tmp <- res.from.AOV$lsd.result
    for (i in 1:length(tmp))
    {
      #cat(paste(if (i != 1) {"\n"}, names(tmp[i]), "\n", sep = ""))
      cat(paste("\n", names(tmp[i]), "\n", sep = ""))
      cat(write.csv2(x = tmp[[i]]))
    }
    sink()
    if (print.dir == TRUE) {
      cat("\nLSD Results were recorded :\n", normalizePath(path), "\n")
    }
  }
}
