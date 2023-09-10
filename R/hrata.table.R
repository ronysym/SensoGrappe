#' hrata.table : Compute the average table for HRATA or HCATA multidimensionnal result
#'
#' @description Compute the dravnieks or mean table from HRATA results or the contingency table for HCATA as defined in Koenig et al (2023)
#'
#' @param	res.agreg results from [hrata.agregation]
#' @param	select.judge logical. (By default = TRUE) if TRUE subjects who did not consider an attribute were excluded from the analysis of that attribute so that the number of subjects for each attribute varied. If FALSE : all subjects are used fo all attributes
#' @param type  aggregation criterion of the average table ("mean" or "dravnieks" or "contingency") by default "dravnieks"
#'
#'
#' @import FactoMineR stringr
#' @rawNamespace import(factoextra, except=hcut)
#' @seealso [hrata.signi()] [hrata.agregation()] [hrata.multidim()]
#'
#'
#' @examples
#'data(rose)
#'data(rose.attribute)
#'res.agreg<-hrata.agregation(data=rose,h.table=rose.attribute,crit.agreg=max)
#'res.signi<-hrata.signi(res.agreg,seuil=0.05)
#'res.table<-hrata.table(res.agreg,type="dravnieks")
#'res.PCA<-hrata.multidim(res.table,method="PCA",scale.unit=FALSE,niv=3)
#'
#'
#' @export


hrata.table <- function(res.agreg,
                        select.judge = FALSE,
                        type = "dravnieks")
{
  lfam.name <- res.agreg$lfam.name
  lcat.name <- res.agreg$lcat.name

  res.agreg$hierarchical.data$Produit = as.factor(res.agreg$hierarchical.data$Produit)
  npdt <- nlevels(res.agreg$hierarchical.data$Produit)

  data <- res.agreg$hierarchical.data

  latt <- vector("list", ncol(data) - 2)
  for (i in 3:ncol(data)) {
    if (select.judge == TRUE) {
      # verifie s'il y a des duplicats dans les sujets selectionnes
      suj <-
        data[which(data[, i] > 0), 1]   # selectionne les sujets qui ont evalue l'attribut i pour n'importe quel produit
    } else{
      suj <-
        data[which(data[, i] >= 0), 1] # si duplicats, on les enleve --> on obtient la liste des sujets qui ont selectionne l'atttribut i pour au moins un des produits
    }

    if (length(which(duplicated(suj))) == 0) {
      # verifie s'il y a des duplicats dans les sujets selectionnes
      suj <- suj
    } else{
      suj <-
        suj[-which(duplicated(suj))] # si duplicats, on les enleve --> on obtient la liste des sujets qui ont selectionne l'atttribut i pour au moins un des produits
    }
    # selection des sujets qui ont au moins mis 1 note d'intensit? ? l'attribut pour un des produits
    vec <- NULL
    for (s in 1:length(suj)) {
      vec <- c(vec, which(data[, 1] == suj[s]))
    }
    latt[[i - 2]] <- data[vec, c(1, 2, i)]
  }


  if (type == "mean") {
    # a modifier sur la base de latt

    data.table <- matrix(nrow = npdt, ncol = ncol(data) - 2)
    for (i in 1:length(latt)) {
      for (k in 1:nlevels(data$Produit)) {
        data.table[k, i] <-
          mean(latt[[i]][which(latt[[i]][, 2] == levels(data$Produit)[k]),][, 3])
      }
    }
    data.table[is.na(data.table)] <- 0

    colnames(data.table) <- colnames(data)[3:ncol(data)]
    rownames(data.table) <- levels(data[, 2])
    data.table = as.data.frame(data.table)
    res.table <- list(data.table, lfam.name, lcat.name)
    names(res.table) <- c("data.table", "lfam.name", "lcat.name")
    return(res.table)

  } else{
    if (type == "dravnieks") {
      # calcul du score de dravnieks
      data.table <- matrix(nrow = npdt, ncol = ncol(data) - 2)
      echelle <- max(data[(3):ncol(data)])

      for (i in 1:length(latt)) {
        for (k in 1:nlevels(data$Produit)) {
          pik <-
            length(which(latt[[i]][latt[[i]][, 2] == levels(data$Produit)[k], 3] > 0)) # nombre de sujets ayant donn? une note positive ? l'attribut i pour le produit k
          pk <-
            length(latt[[i]][latt[[i]][, 2] == levels(data$Produit)[k], 3]) # nombre de sujets ayant ?valu? l'attribut i pour au moins 1 produit
          xi <-
            sum(latt[[i]][latt[[i]][, 2] == levels(data$Produit)[k], 3]) # somme des notes de l'attribut i pour le produit k

          x <-
            echelle * pk # somme des notes max pour les sujets qui ont ?valu? l'attribut i pour un moins un  des produits

          data.table[k, i] <-
            sqrt((pik / pk * 100) * (xi / x * 100))
        }
      }
      data.table[is.na(data.table)] <- 0
      colnames(data.table) <- colnames(data)[3:ncol(data)]
      rownames(data.table) <- levels(data[, 2])
      data.table <- as.data.frame(data.table)
      res.table <- list(data.table, lfam.name, lcat.name)
      names(res.table) <- c("data.table", "lfam.name", "lcat.name")
      return(res.table)

    } else{
      if (type == "contingency") {
        lfam.name <- res.agreg$lfam.name
        lcat.name <- res.agreg$lcat.name
        data.hcata <- canonisation(res.agreg$hierarchical.data, 2)
        data.hcata$Score[data.hcata$Score > 1] <- 1
        data.hcata$Score[data.hcata$Score == 0] <- NA

        #creation du tableau de contingence
        mytable <-
          xtabs(Score ~ Attribute + Produit, data = data.hcata)
        attributes(mytable)$class <- "table"
        mytable <- addmargins(mytable, 2, FUN = list(list(
          Sum = sum,
          Max = max,
          Min = min
        )), quiet = TRUE)
        mytable <-
          cbind(mytable, mytable[, "Max"] - mytable[, "Min"])
        colnames(mytable)[length(colnames(mytable))] = "Amplitude"
        mytable <- as.data.frame(mytable)

        data.table <- as.data.frame(mytable)
        res.table <- list(data.table, lfam.name, lcat.name)
        names(res.table) <-
          c("data.table", "lfam.name", "lcat.name")
        return(res.table)

      } else{
        print("Type not valid")
      }
    }
  }
}
