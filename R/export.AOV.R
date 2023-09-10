#' export.AOV : Export results from AOV.Grappe
#'
#' @description Export results from [AOV.Grappe()]
#'
#' @param res.from.AOV   results from [AOV.Grappe()]
#' @param	save.stat save pvalue, signif, F, DF et models results. TRUE by default
#' @param	save.LSD  save LS means results. TRUE by defaultTRUE by default
#' @param	name      name of the existing files
#' @param	name.stat name for the Stat Files (defaut is NULL)
#' @param	name.LSD  name for the LSD Files (defaut is NULL)
#' @param	print.dir print the directory name. TRUE by default
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
#' res.AOV <- AOV.Grappe(x = winef, column = c(3:4), "ProductName + (1|CJ)+ (1|ProductName:CJ)")
#' \dontrun{
#' export.AOV(res.AOV)
#' }
#' mycolor <- c("#EDF8E9", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#005A32")
#' graph.AOV(res.AOV, color.graph = mycolor, y.label = "Moyenne")
#' graph.AOV(res.AOV, title.graph = "Sensory Score", add.moy = TRUE, x.ordered = 1, ymin = 1, ymax = 9)
#'
#' @export
export.AOV <- function(res.from.AOV, save.stat = TRUE, save.LSD = TRUE, name = substitute(res.from.AOV), name.stat = NULL, name.LSD = NULL, print.dir = TRUE) {
  if (is.null(name.stat) == TRUE) {
    name.stat <- "stat"
  }
  if (is.null(name.LSD) == TRUE) {
    name.LSD <- "LSD_test"
  }


  # Verifie que les noms de fichiers soient differents
  if (save.stat == TRUE && save.LSD == TRUE && name.stat == name.LSD) {
    stop("Arrete de me tester ! Les 2 fichiers doivent avoir un nom different... STP !", call. = FALSE)
  }

  # Enregistre les stats associees a l'ANOVA
  if (save.stat == TRUE) {
    # Ecrit les sorties dans un fchier .csv
    sink(file = paste(Sys.Date(), "_", name, "_", name.stat, ".csv", sep = ""))
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
      cat("\nLes stats ont ete enregistrees :\n", getwd(), "\n")
    }
  }

  # Idem avec les LSD
  if (save.LSD == TRUE) {
    sink(file = paste(Sys.Date(), "_", name, "_", name.LSD, ".csv", sep = ""))
    # Extrait les LSD dans un objet temporaire car les LSD constituent une sous liste
    # (-> Remonte les LSD d'un niveau)
    tmp <- res.from.AOV$lsd.result
    for (i in 1:length(tmp))
    {
      cat(paste(if (i != 1) {
        "\n"
      }, names(tmp[i]), "\n", sep = ""))
      cat(write.csv2(x = tmp[[i]]))
    }
    sink()
    if (print.dir == TRUE) {
      cat("\nLes LSD ont ete enregistres :\n", getwd(), "\n")
    }
  }
}
