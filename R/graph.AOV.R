#' graph.AOV : Plot Means associated to AOV.Grappe function
#'
#' @description Plotting method for results obtain with AOV.Grappe. Mean value are represented associated with post-hoc test results done using lmerTest. Fixed Factors and Interactions can be drawn
#'
#' @param	AOV.res  an object results from [AOV.Grappe()]
#' @param	factor   factor is used to precise the fixed factor(s) to be presented. NULL by default
#'                 - factor=NULL then all factors are presented
#'                 - to represent one factor use  factor="ProductName"
#'                 - to represent several factos use   factor=c("ProductName", "Block", "Seance")
#' @param	attribute   attribute is used to precise the attribute(s) to be presented. NULL by default
#'                 - Attribute=NULL then all attributes are presented
#'                 - to represent one attribute use  attribute="S_sucre"
#'                 - to represent several attributes use   attribute=c("S_sucre", "A_abricot")
#'
#' @param	horizontal logical.draw horizontal graphs ( FALSE by default)
#' @param	ymin minimum for Y (0 by default)
#' @param	ymax maximum  for Y (by default the max of Y is used)
#' @param	angle.x angle for labels on X (by default 0) but 45 is better for long labels
#' 								- if horizontal=TRUE, then labels are oriented automatically
#' @param	angle.y angle for labels on y (by default 0)
#' @param	angle.grp angle for the LSD test letters over the histogram (by default 0)
#' @param	size.x size of the fonts for abscisse
#' @param	size.grp size of the fonts for the group letters
#' @param	save.graph logical.Save the graph in the working directory  ? FALSE by default
#' 							-to access the working directory you can use : getwd()
#' @param	prefix prefix for the name of the saved files
#' @param	suffix suffix for the name of the saved files
#' @param	extension image format (wmf, png, etc.) wmf by default
#' @param pvalue logical.add the pvalue of the representated factor on the graph ? TRUE by default
#' @param title.graph Value to be given for all title of each graph. By default, title.graph = NULL makes each title presenting the attribute name
#' @param x.ordered  ordered abscisse axis (by default = 3)
#                    1: alphabetically,
#                    2: decreasing averages,
#                    3: increasing averages.
#' @param add.moy  logical.  add the value of the mean on the graph (FALSE by default)
#' @param color.graph vector containing as much color than needed (NULL by defaut)
#' @param y.label value for the label (by default : "Mean")
#' @param x.label value for the label (by default : "Product")
#'
#'
#' @return Returns graphs of means by factors and variables with letters from posthoc test, standard deviations and pvalue from ANOVAs
#'
#'
#' @seealso [AOV.Grappe()]
#'
#' @import  utils ggplot2 grDevices tidyr qpdf RColorBrewer randomcoloR
#' @rawNamespace import(stats, except=step)
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
#' @details
#' By default, the function uses the 20 colors from the command = c(brewer.pal(12,"Set3"),brewer.pal(8,"Set1")) that is recorded in the function and added with as much random color than needed over the 20 first color
#' It is possible to modify by entering a color vector containing at least the same number of colors to use in the graph
#' For example :  color.graph= rep("grey",6)
#'
#' @export
graph.AOV <- function(AOV.res = NULL,
                      factor = NULL,
                      attribute = NULL,
                      horizontal = FALSE,
                      pvalue = TRUE,
                      title.graph = NULL,
                      ymin = 0,
                      ymax = NULL,
                      angle.x = 0,
                      angle.y = 0,
                      angle.grp = 0,
                      size.x = 10,
                      size.grp = 4,
                      save.graph = FALSE,
                      x.ordered = 3,
                      prefix = NULL,
                      suffix = NULL,
                      add.moy = FALSE,
                      extension = "wmf",
                      color.graph = NULL,
                      y.label = "Mean",
                      x.label = NULL) {


   # Variables pour aligner les noms sur les axes
  hx <- hy <- vx <- vy <- hgrp <- 0.5
  if (angle.x > 0) {
    hx <- 1
  }
  if (angle.x < 0) {
    hx <- 0
  }
  if (angle.x == 90) {
    vx <- 0.5
  } else {
    vx <- 1
  }
  if (angle.y != 0) {
    vy <- 0.5
  }
  if (angle.y == 90) {
    hy <- 0.5
  } else {
    hy <- 1
  }
  if (angle.grp != 0) {
    hgrp <- 0
  }

  v1 <- v2 <- Moy_num <- Names <- SE_num <- NULL

  # Explication de comment utiliser la fonction ; Les donnees doivent etre l'objet lsd.result retourne par AOV.Grappe
  texte <- "L'argument AOV.result ne doit pas etre vide.
	Veillez a :
	1. Enregistrer le resultat de la fonction AOV.grappe dans un objet
	2. Indiquer cet objet dans l'argument AOV.result
	Exemple :
	res <- AOV.Grappe(...)
	Graph.AOV(AOV.res=res)"
  if (is.null(AOV.res)) {
    stop(texte)
  }

  # Verifie que l'objet indique contient les resultats des tests LSD
  verif <- grep(pattern = "lsd.result", x = attr(AOV.res, "names"))
  if (length(verif) == 0) {
    stop(texte)
  } else {
    AOV.pvalue <- AOV.res[1]
    AOV.result <- AOV.res[[verif]]
  }

  # Differents facteurs demandes lors de l'ANOVA
  factor.source <- attr(AOV.result, "names")
  # Definit les facteurs demandes pour les graphs
  if (is.null(factor)) {
    factor <- factor.source
  } else {
    # verifie que les facteurs soient bien ecrits
    for (fv in 1:length(factor))
    {
      verif <- sum(grep(factor[fv], factor.source, fixed = TRUE))
      if (verif == 0) {
        stop("Erreur. Verifiez l'orthographe des facteurs")
      }
    }
  }


  for (f in 1:length(factor))
  {
    # Enregistre les donnees corespondantes a chaque factor
    pvalue.factor <- AOV.pvalue[[1]][factor[f], ]
    data.factor <- AOV.result[[factor[f]]]
    nombaz <- factor[f]
    # Definit les attributs
    if (is.null(attribute)) {
      # Tous les blocks seront utilises pour les graphs
      block <- 1:(dim(data.factor)[2] / 3)
    } else {
      # Verifie d'abord que les attributs soient bien orthographies
      verif <- vector()
      for (a in 1:length(attribute))
      {
        # Cherche les attributs dans les noms de colonnes ("attribute" ou "attribute (AOV)")
        verif[1] <- sum(grep(attribute[a], colnames(data.factor), fixed = TRUE))
        verif[2] <- sum(grep(paste(attribute[a], " (AOV)"), colnames(data.factor), fixed = TRUE))
        # Arrete si mal orthographie
        if (sum(verif) == 0) {
          stop("Erreur : Verifiez l'orthographe des attributs")
        }
      }
      # Si tout est ok, extrait les blocks filtres
      res.col <- vector()

      for (a in 1:length(attribute))
      {
        colnum <- grep(attribute[a], colnames(data.factor), fixed = TRUE)
        if (length(res.col) == 0) {
          res.col <- colnum
        } else {
          res.col <- paste(res.col, colnum)
        }
      }

      # Transforme le vecteur texte en nombre
      if (sum(grep(" ", res.col)) == 0) {
        block <- as.numeric(res.col)
      } else {
        block <- as.numeric(unlist(strsplit(x = res.col, split = " ", fixed = TRUE)))
      }
      block <- ceiling(block / 3)
    }


    i=1
    # Boucle pour les graphiques
    for (i in block)
    {
      # Enregistre la pvalue e la variable etudiee
      pvalue.temp <- pvalue.factor[[i]]
      if (pvalue.temp < 0.0001) {
        pvalue.temp <- "< 0.0001"
      }


      # Extrait les colonnes par bloc de 3 (d'après la structure de ...$lsd.result)
      col <- (1:3) + 3 * (i - 1)
      data.temp <- data.factor[, col]
      # Enregistre le nom de la variable etudiee

      if (is.null(title.graph) == TRUE) {
        name.temp <- colnames(data.temp)[1]
      } else {
        name.temp <- title.graph
      }

      # Supprime les espaces dans les noms de groupes
      data.temp$Grp <- gsub(" ", "", data.temp$Grp)
      # Transforme les facteurs en nombres
      data.temp$SE_num <- data.temp$Moy_num <- 0
      for (n in 1:length(data.temp$Moy_num))
      {
        data.temp$Moy_num[n] <- as.numeric(levels(factor(data.temp[n, 1])))
        data.temp$SE_num[n] <- as.numeric(levels(factor(data.temp[n, 2])))
      }
      # Extrait les noms de produits puor pouvoir faire des tris
      data.temp <- cbind(rownames(data.temp), data.temp)
      colnames(data.temp)[1] <- "Names"
      # Mise en forme differente en fonction de l'orientation demandee du graphique
      if (horizontal == TRUE) {
        hgrp <- angle.x <- 0
        hx <- 0.5
      }

      if (is.null(color.graph) == FALSE) {col.graph=color.graph}  else if (length(data.temp$Names)<=20){
        col.graph <- c(brewer.pal(12, "Set3"), brewer.pal(8, "Set1"))
        }else{ col.graph <- distinctColorPalette(length(data.temp$Names))}

      data.temp$Names <- factor(data.temp$Names)
      names(col.graph) <- levels(data.temp$Names)

      # Ordonne les niveaux par moyennes croissantes
      if (x.ordered == 2) {
        data.temp$Names <- factor(data.temp$Names, levels = data.temp$Names[order(data.temp[, 2], decreasing = TRUE)])
      } else

      # Ordonne les niveaux par ordre alphabetique
      if (x.ordered == 3) {
        data.temp$Names <- factor(data.temp$Names, levels = data.temp$Names[order(data.temp[, 2], decreasing = FALSE)])
      }

      # Calcul la limite max sur Y et l'endroit où positionner les lettres des groupes
      {
        ymx <- ymax
        if (is.null(ymx)) {
          ymx <- ceiling((max(data.temp$Moy_num) + max(data.temp$SE_num)) * 1.1)
        }
        ypos <- mean(c(max(data.temp$Moy_num + data.temp$SE_num), ymx))
        # Dessine le graphique



        gp <- ggplot(data.temp, aes(x = Names, y = Moy_num))





        gp <- ggplot(data.temp, aes(x = Names, y = Moy_num, fill = Names))

        gp <- gp + scale_fill_manual(name = "Names", values = col.graph)
        gp <- gp + geom_bar(position = "dodge", stat = "identity", color = "black") + theme_bw()
        gp


        # Personnalisation du graphique
        # Barres avec Standard Error (donnees par lmer)
        gp <- gp + geom_errorbar(data = data.temp, aes(ymin = Moy_num, ymax = Moy_num + SE_num), width = 0.25)

        # Titres
        if (is.null(x.label) == TRUE) {
          x.label.tmp <- nombaz
        }

        gp <- gp + labs(x = x.label.tmp, y = y.label, title = paste(name.temp), caption = paste("pvalue :", pvalue.temp))

        if (pvalue == FALSE) {
          gp <- gp + labs(x = x.label.tmp, y = y.label, title = paste(name.temp), caption = NULL)
        }


        # Chartre graphique
        gp <- gp + theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 12),
          axis.text.x = element_text(angle = angle.x, hjust = hx, vjust = vx, size = size.x),
          axis.text.y = element_text(angle = angle.y, hjust = hy, vjust = vy, size = size.x),
          axis.title = element_text(size = 14),
          plot.title = element_text(size = rel(2), face = "bold", vjust = 1),
          legend.position = "none"
        )
        # Bornes pour Y
        gp <- gp + coord_cartesian(ylim = c(ymin, ymx))
        # Retourne le graphique a l'horizontal si demande
        if (horizontal == TRUE) {
          gp <- gp + coord_flip()
        }
        # Ajoute les lettres des groupes
        gp <- gp + annotate(geom = "text", x = data.temp$Names, y = ypos, label = data.temp$Grp, size = size.grp, hjust = hgrp, angle = angle.grp)
        if (add.moy == TRUE) {
          gp <- gp + annotate(geom = "text", x = data.temp$Names, y = ymin + 0.2, label = format(data.temp$Moy_num, 2), size = 3, hjust = hgrp, angle = angle.grp)
        }



        # Cree une nouvelle fenetre de graphique (si travail sous R et non RStudio)
        if (names(dev.cur()) == "null device" || names(dev.cur()) == "windows") {
          dev.new()
        }

        # Affiche le graphique final
        print(gp)
      }
      # Remplace les : dans le nom du factor s'il y en a
      fct <- gsub(":", "_", attr(data.factor, "nom"))

      # Engistre le graph (si demande)
      if (save.graph == TRUE) {
        nom <- paste(fct, "_", name.temp, sep = "")
        if (!is.null(prefix)) {
          nom <- paste(prefix, "_", nom, sep = "")
        }
        if (!is.null(suffix)) {
          nom <- paste(nom, "_", suffix, sep = "")
        }
        ggsave(filename = paste(nom, extension, sep = "."), path = getwd(), gp)
      }

      # Ajoute le graphique des interactions
      if (sum(grep(":", factor[f], fixed = TRUE)) != 0) {
        nomBaz <- unlist(strsplit(x = factor[f], split = ":", fixed = TRUE))
        data.temp <- separate(data.temp, Names, c("v1", "v2"), sep = ":")
        #
        { # Dessine le graphique des interactions
          plot.inter <- ggplot(data = data.temp, aes(x = v1, y = Moy_num, colour = v2, group = v2))
          plot.inter <- plot.inter + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line")
          plot.inter <- plot.inter + theme_bw()
          # Titres
          plot.inter <- plot.inter + labs(x = nomBaz[1], y = y.label, title = name.temp, colour = nomBaz[2], caption = paste("pvalue :", pvalue.temp))

          if (pvalue == FALSE) {
            plot.inter <- plot.inter + labs(x = nomBaz[1], y = y.label, title = name.temp, colour = nomBaz[2], caption = NULL)
          }


          # Chartre graphique
          plot.inter <- plot.inter + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = angle.x, hjust = hx, vjust = vx, size = size.x),
            axis.text.y = element_text(angle = angle.y, hjust = hy, vjust = vy),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = rel(2), face = "bold", vjust = 1)
          )
          # Bornes pour Y
          plot.inter <- plot.inter + coord_cartesian(ylim = c(ymin, ymx)) #+ ylim(c(ymin, ymx))

          # Cree une nouvelle fenetre de graphique (si travail sous R et non RStudio)
          if (names(dev.cur()) == "null device" || names(dev.cur()) == "windows") {
            dev.new()
          }

          # Retourne le graphique a l'horizontal si demande
          if (horizontal == TRUE) {
            plot.inter <- plot.inter + coord_flip()
          }
          print(plot.inter)

          # Engistre le graph (si demande)
          if (save.graph == TRUE) {
            nom <- paste(fct, "_", name.temp, "_interaction", sep = "")
            if (!is.null(prefix)) {
              nom <- paste(prefix, "_", nom, sep = "")
            }
            if (!is.null(suffix)) {
              nom <- paste(nom, "_", suffix, sep = "")
            }
            ggsave(filename = paste(nom, extension, sep = "."), path = getwd(), plot.inter)
          }
        }

        { # Dessine le graphique des interactions
          plot.inter <- ggplot(data = data.temp, aes(x = v2, y = Moy_num, colour = v1, group = v1))
          plot.inter <- plot.inter + stat_summary(fun = mean, geom = "point") + stat_summary(fun = mean, geom = "line")
          plot.inter <- plot.inter + theme_bw()
          # Titres
          plot.inter <- plot.inter + labs(x = nomBaz[2], y = y.label, title = name.temp, colour = nomBaz[1], caption = paste("pvalue :", pvalue.temp))

          if (pvalue == FALSE) {
            plot.inter <- plot.inter + labs(x = nomBaz[2], y = y.label, title = name.temp, colour = nomBaz[1], caption = NULL)
          }


          # Chartre graphique
          plot.inter <- plot.inter + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = angle.x, hjust = hx, vjust = vx),
            axis.text.y = element_text(angle = angle.y, hjust = hy, vjust = vy),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = rel(2), face = "bold", vjust = 1)
          )
          # Bornes pour Y
          plot.inter <- plot.inter + coord_cartesian(ylim = c(ymin, ymx)) #+ ylim(c(ymin, ymx))

          # Cree une nouvelle fenetre de graphique (si travail sous R et non RStudio)
          if (names(dev.cur()) == "null device" || names(dev.cur()) == "windows") {
            dev.new()
          }

          # Retourne le graphique a l'horizontal si demande
          if (horizontal == TRUE) {
            plot.inter <- plot.inter + coord_flip()
          }
          print(plot.inter)

          # Engistre le graph (si demande)
          if (save.graph == TRUE) {
            nom <- paste(fct, "_", name.temp, "_interaction", sep = "")
            if (!is.null(prefix)) {
              nom <- paste(prefix, "_", nom, sep = "")
            }
            if (!is.null(suffix)) {
              nom <- paste(nom, "_", suffix, sep = "")
            }
            ggsave(filename = paste(nom, extension, sep = "."), path = getwd(), plot.inter)
          }
        }
      }
    }
  }
  # Affiche le chemin du dossier où ont ete enregistres les graphiques
  if (save.graph == TRUE) {
    cat("\nGraphics have been saved in:\n")
    print(getwd())
  }
}
