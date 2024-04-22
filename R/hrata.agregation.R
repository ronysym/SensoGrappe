#' hrata.agregation : prepare the HRATA or HCATA table for analysis from rawdata
#'
#' @description create the dataframe used for HRATA or HCATA analysis by integreting the reallocation strategy from attribute to category and category to term as defined in Koenig et al (2023)
#'
#'
#' @param	data  a dataframe containing rawdata HRATA or HCATA sensory task with Juge in first column and Product in the second column then all attributes
#' @param	h.table  a dataframe with the hierarchical structure : three columns "Family", "Category", "Attribute"
#' @param	crit.agreg agregation criterion used during the data allocation : "max" or "mean" (by default : max is used in Koenig et al 2023 and is adapted for HRATA and HCATA, mean is only usable in HRATA)
#'
#'
#' @return List of 3 objects
#'          1) hierarchical.data : a dataframe of the reallocated data.
#'          2) lfam.name  a list of the families and associated categories.
#'          3) lcat.name a list of the categories and associated attributes.
#'
#'
#' @references Koenig, L., Symoneaux, R., Pouzalgues, N., Cariou, V., Vigneau, E., & Coulon-Leroy, C. (2023). Development of a Hierarchical Rate-All-That-Apply (HRATA) methodology for the aromatic characterisation of wine. OENO One, 57(1), 175–190. https://doi.org/10.20870/oeno-one.2023.57.1.5531
#'
#' @seealso [hrata.signi()] [hrata.table()] [hrata.multidim()]
#'
#' @examples
#' data(rose)
#' data(rose.attribute)
#' res.agreg <- hrata.agregation(data = rose, h.table = rose.attribute, crit.agreg = "max")
#' res.signi <- hrata.signi(res.agreg, seuil = 0.05)
#' res.table <- hrata.table(res.agreg, type = "dravnieks")
#' res.PCA <- hrata.multidim(res.table, method = "PCA", scale.unit = FALSE, niv = 3)
#'
#' @export
hrata.agregation <- function(data, h.table, crit.agreg = "max") {


  # Check if 'data' is a data.frame
  if (!is.data.frame(data)) {
    stop("The 'data' argument must be a data frame.", call. = FALSE)
  }

  # Check if 'h.table' is a data.frame
  if (!is.data.frame(h.table)) {
    stop("The 'h.table' argument must be a data frame.", call. = FALSE)
  }


  # Check if 'crit.agreg' is either "max" or "mean"
  allowed_crit_agreg <- c("max", "mean")
  if (!crit.agreg %in% allowed_crit_agreg) {
    stop("The 'crit.agreg' argument must be either 'max' or 'mean'.", call. = FALSE)
  }

  # Check if 'data' contains at least four columns
  if (ncol(data) < 4 ) {
    stop("The 'data' argument must contain at least four columns: 'Judge' and 'Product' and 2 attribute", call. = FALSE)
  }

  # Check if 'h.table' contains three columns
  if (ncol(h.table) != 3) {
    stop("The 'h.table' argument must contain three columns: 'Family', 'Category', and 'Attribute'.", call. = FALSE)
  }

  # Check column names in 'data'
  allowed_judge_cols <- c("CJ", "Judge", "Juge")
  allowed_product_cols <- c("Product", "Produit")

  if (!colnames(data)[1] %in% allowed_judge_cols || !colnames(data)[2] %in% allowed_product_cols) {
    stop("The first column of the 'data' argument must be named 'CJ', 'Judge', or 'Juge', and the second column must be named 'Product' or 'Produit'.", call. = FALSE)
  }

  # Check if 'h.table' column names are either English or French
  allowed_h_table_cols_en <- c("Family", "Category", "Attribute")
  allowed_h_table_cols_fr <- c("Famille", "Categorie", "Attribut")

  if (!all(colnames(h.table) %in% allowed_h_table_cols_en) & !all(colnames(h.table) %in% allowed_h_table_cols_fr)) {
    stop("The 'h.table' column names must be either 'Family', 'Category', 'Attribute' or 'Famille', 'Categorie', 'Attribut'", call. = FALSE)
  }

  #Check if h.table and data match


  vtmp <- c()
  # Scan the lines of the h.table object
  for (i in 1:nrow(h.table)) {
    # If the Family term changes
    if (i == 1 || h.table$Famille[i] != h.table$Famille[i-1]) {
      vtmp <- c(vtmp, h.table$Famille[i])
    }
    # If the Category term changes
    if (i == 1 || h.table$Categorie[i] != h.table$Categorie[i-1]) {
      vtmp <- c(vtmp, h.table$Categorie[i])
    }
    # Adding the term Attribute
    vtmp <- c(vtmp, h.table$Attribut[i])
  }


  if (length(names(data)[3:length(data)])!=length(vtmp)) {
    stop("The 'h.table' and 'data' don't match for Familly or Category")
  }

  if (any(names(data)[3:length(data)]!=vtmp)) {
    stop("The 'h.table' and 'data' don't match, please verify")
  }



  data <- Var.Grappe(data, 3:length(data), "numeric", verbose = FALSE)
  data <- Var.Grappe(data, column = 1:2, type = "factor", verbose = FALSE)

  codage <- hrata.codage(h.table)

  natt <- ncol(data) - 2 # nombre d'attributs (famille+categorie+termes)
  npdt <- nlevels(data$Produit) # nombre de produits
  nsujet <- nlevels(data$Juge) # nombre de sujets
  nniv <- 3 # nombre de niveaux hierarchiques
  nfam <- max(codage$famille) # nombre de famille
  ncat <- max(codage$categorie, na.rm = TRUE) # nombre de categories


  ################################################################
  ## listes pour recuperer les colonnes des categories et familles

  lfam <- vector("list", nfam) # liste avec les numeros de colonnes des categories dans les familles
  lfam.name <- vector("list", nfam) # liste avec les noms des attributs des categories dans les familles
  lcatinfam <- vector("list", nfam) # liste avec les numeros des categories et des familles
  lcat <- vector("list", ncat) # liste avec les numeros de colonnes des termes dans les categories
  lcat.name <- vector("list", ncat) # liste avec le noms des attributs des termes dans les categories
  cat <- 0
  for (fam in 1:nfam) {
    elim <- c()
    loc <- which(codage$famille == fam)
    elim <- which(is.na(codage[loc, 2]))
    if (length(elim) > 0) loc <- loc[-elim]
    numcat <- c(unique(codage[loc, 2]))
    nc <- length(numcat)
    for (c in 1:nc) {
      locc <- which(codage$categorie == numcat[c])
      lfam[[fam]] <- c(lfam[[fam]], locc[1])
      lfam.name[[fam]] <- as.character(codage$attribut[lfam[[fam]]])
      lcatinfam[[fam]][[c]] <- locc[-1]
      cat <- cat + 1
      lcat[[cat]] <- locc[-1]
      lcat.name[[cat]] <- as.character(codage$attribut[lcat[[cat]]])
    }
  }

  names(lfam.name) <- as.character(codage$attribut[codage$type == "F"])

  names(lcat.name) <- as.character(codage$attribut[codage$type == "C"])

  if (all(is.na(names(lfam.name)))) {
    names(lfam.name) <- names(lcat.name)
  } # si pas de famille alors on double les categories

  ######################
  ## Construction des tableaux des attributs, categories et familles

  # on passe tous les NA en 0
  data[is.na(data)] <- 0
  hierarchical.data <- data

  # matrice avec les donnees termes
  data.att <- data[, unlist(lcatinfam) + 2]
  data.att <- cbind(data[, 1:2], data.att)
  hierarchical.term <- data.att

  # matrice avec les donnees categories
  data.cat <- matrix(nrow = nrow(data), ncol = ncat)
  i <- 1
  for (f in 1:nfam) {
    nc <- length(lcatinfam[[f]])
    for (c in 1:nc) {
      datc <- cbind(data[, lfam[[f]][c] + 2], data[, lcatinfam[[f]][[c]] + 2])
      data.cat[, i] <- apply(datc, 1, crit.agreg)
      i <- i + 1
    }
  }
  colnames(data.cat) <- as.character(codage$attribut[codage$type == "C"])
  data.cat <- cbind(data[, 1:2], data.cat)
  hierarchical.cat <- data.cat

  # matrice avec les donnees familles


  if (length(names(lfam.name)) == length(names(lcat.name))) { # si pas de famille alors on bouble les categories pour les familles
    hierarchical.fam <- hierarchical.cat
  } else {
    data.fam <- matrix(nrow = nrow(data), ncol = nfam)
    for (f in 1:nfam) {
      datf <- cbind(data[, which(codage$type == "F")[f] + 2], data[, lfam[[f]] + 2], data[, unlist(lcatinfam[[f]]) + 2])
      data.fam[, f] <- apply(datf, 1, crit.agreg)
    }
    colnames(data.fam) <- codage$attribut[codage$type == "F"]
    data.fam <- cbind(data[, 1:2], data.fam)
    hierarchical.fam <- data.fam
  }

  hierarchical.data <- cbind(hierarchical.fam, hierarchical.cat[, 3:ncol(hierarchical.cat)], hierarchical.term[, 3:ncol(hierarchical.term)])
  res.agreg <- list(hierarchical.data, lfam.name, lcat.name)
  names(res.agreg) <- c("hierarchical.data", "lfam.name", "lcat.name")
  class(res.agreg) <- c("list", "hrata")

  return(res.agreg)
}

