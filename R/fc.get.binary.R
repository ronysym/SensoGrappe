#' Binarisation des données de commentaires libres
#'
#' @description
#' Construit une matrice binaire (sujets × produits × descripteurs) à
#' partir des données de commentaires libres, avec complétion des
#' évaluations manquantes et filtrage des descripteurs peu fréquents.
#'
#' @param res.algo Un `data.frame` contenant au minimum les colonnes
#'   `subject`, `product` et `descripteur`. Peut également contenir
#'   `dimension`, `concept` et `dimension.concept` selon le flux de
#'   traitement.
#' @param common.level Caractère. Niveau hiérarchique à récupérer pour
#'   chaque descripteur conservé. Doit être `"concept"`, `"dimension"`
#'   ou `"any"`. Par défaut `"concept"`.
#' @param balanced.data Logique. Si `TRUE` (défaut), les données sont
#'   équilibrées : les évaluations manquantes sont complétées par des
#'   lignes de zéros.
#' @param manual Logique. Si `TRUE`, indique que les données proviennent
#'   d'un codage manuel. Influence la récupération du niveau commun.
#'   Par défaut `FALSE`.
#' @param specific_threshold Numérique. Proportion minimale d'observations
#'   par rapport à Ep requise dans un produit pour qu'un descripteur soit
#'   considéré comme présent dans ce produit. Par défaut `0.05`.
#' @param specific_coverage Entier. Nombre minimum de produits devant
#'   satisfaire `specific_threshold` pour qu'un descripteur soit conservé.
#'   Par défaut `1`.
#'
#' @return Une liste contenant :
#'   \describe{
#'     \item{`dta`}{`data.frame` binaire avec les colonnes `subject`,
#'       `product` et une colonne par descripteur conservé (valeurs 0/1).}
#'     \item{`common`}{Vecteur nommé donnant le niveau commun
#'       (selon `common.level`) de chaque descripteur conservé.
#'       Vaut `"idem"` pour tous les descripteurs si le niveau n'est
#'       pas disponible.}
#'   }
#'
#' @details
#' Les étapes internes sont :
#' \enumerate{
#'   \item Création d'un tableau de contingence (évaluation × descripteur)
#'     et binarisation (présence/absence).
#'   \item Complétion des évaluations manquantes (si `balanced.data = TRUE`).
#'   \item Filtrage des descripteurs selon `specific_threshold` et
#'     `specific_coverage`.
#'   \item Récupération du niveau hiérarchique commun pour chaque
#'     descripteur retenu.
#' }
#'
#' Le paramètre `Ep` correspond au nombre de sujets attendus par produit :
#' en mode équilibré (`balanced.data = TRUE`), il est fixé au nombre total
#' de sujets ; sinon, il reflète les observations réelles.
#'
#' @seealso \code{\link{data.preprocess.fc}}, \code{\link{transfert.MV.BM}}
#'
#' @examples
#' \dontrun{
#' binary_result <- get.binary(
#'   res.algo           = res_algo_df,
#'   common.level       = "concept",
#'   balanced.data      = TRUE,
#'   specific_threshold = 0.05,
#'   specific_coverage  = 1
#' )
#'
#' binary_result$dta     # Matrice binaire
#' binary_result$common  # Niveau commun par descripteur
#' }
#'
#' @export
#' 
#' 


get.binary <- function(
    res.algo,
    common.level = "concept",
    balanced.data = TRUE,
    manual = FALSE,
    specific_threshold = 0.05,
    specific_coverage = 1
) {
  
  # =============================================
  # ÉTAPE 1 : PRÉPARATION DES DONNÉES INITIALES
  # =============================================
  
  evals <- paste(res.algo$subject, res.algo$product, sep = "et")
  
  bin <- table(evals, res.algo$descripteur)
  class(bin) <- "matrix"
  bin[bin > 1] <- 1
  
  lsplit <- gregexpr("et", rownames(bin))
  get.s <- function(ou) {
    ls <- lsplit[[ou]]
    substring(rownames(bin)[ou], 1, ls[[1]] - 1)
  }
  get.p <- function(ou) {
    ls <- lsplit[[ou]]
    substring(rownames(bin)[ou], ls[[1]] + 2)
  }
  
  retour.1 <- data.frame(
    subject = sapply(1:nrow(bin), get.s),
    product = sapply(1:nrow(bin), get.p)
  )
  retour.1 <- as.data.frame(cbind(retour.1, bin))
  rownames(retour.1) <- as.character(1:nrow(retour.1))
  retour.1$subject <- as.factor(retour.1$subject)
  retour.1$product <- as.factor(retour.1$product)
  
  # =============================================
  # ÉTAPE 2 : COMPLÉTION DES DONNÉES MANQUANTES
  # =============================================
  
  get.Ep <- table(res.algo$subject, res.algo$product) > 0
  Ep <- colSums(get.Ep)
  if (balanced.data) {
    Ep <- rep(nrow(get.Ep), length(Ep))
  }
  
  if (balanced.data & any(as.numeric(table(retour.1$product)) < Ep)) {
    for (pp in levels(retour.1$product)) {
      if (sum(retour.1$product == pp) < Ep[1]) {
        ajout.0 <- matrix(0, Ep[1] - sum(retour.1$product == pp), ncol(retour.1) - 2)
        colnames(ajout.0) <- colnames(retour.1)[-c(1:2)]
        croise <- as.matrix(table(retour.1$subject, retour.1$product))
        miss.subj <- rownames(croise)[croise[, pp] == 0]
        ajout <- data.frame(subject = miss.subj, product = rep(pp, length(miss.subj)), ajout.0)
        colnames(ajout) <- colnames(retour.1)
        retour.1 <- rbind.data.frame(retour.1, ajout)
        retour.1 <- retour.1[order(retour.1$subject, retour.1$product), ]
      }
    }
  }
  
  # =============================================
  # ÉTAPE 3 : FILTRAGE FINAL
  # =============================================
  
  cont <- aggregate(. ~ product, retour.1, sum)
  rownames(cont) <- cont$product
  cont <- cont[, -c(1:2)]
  
  vec_specific <- apply(cont / Ep >= specific_threshold, 2, sum)
  kept_specific <- vec_specific >= specific_coverage
  
  kept <- colnames(cont)[kept_specific]
  
  if (length(kept) == 0) {
    warning("Aucun descripteur conservé avec ces critères !")
  }
  
  kept <- c("subject", "product", kept)
  retour.1 <- retour.1[, colnames(retour.1) %in% kept]
  
  # =============================================
  # ÉTAPE 4 : RÉCUPÉRATION DU NIVEAU COMMUN
  # =============================================
  
  
  vec <- colnames(retour.1)[-c(1:2)]
  
  if (!manual | common.level %in% names(res.algo)) {
    # Récupération du niveau commun pour chaque descripteur
    get.common <- function(mot) {
      res.algo[match(mot, res.algo$descripteur), common.level]
    }
    retour.2 <- unlist(sapply(vec, get.common))
    names(retour.2) <- as.character(seq_along(retour.2))
    
  } else {
    # Niveau commun non disponible : valeur par défaut
    retour.2 <- rep("idem", length(vec))
    names(retour.2) <- as.character(seq_along(retour.2))
  }
  
  return(list(dta = retour.1, common = retour.2))
}


##################################################
#' Décomposition d'un descripteur hiérarchique (format ConsoTextPlorer)
#'
#' @description
#' Décompose une chaîne de caractères au format ConsoTextPlorer
#' (`"dimension:concept/descripteur"`) en ses trois composantes :
#' dimension, concept et descripteur.
#'
#' @param x Caractère. Chaîne à décomposer, au format
#'   `"dimension:concept/descripteur"`. Peut être `NA`.
#' @param tres Logique. Si `FALSE` (défaut), le suffixe `_très` est
#'   supprimé des composantes `concept` et `descripteur`.
#'   Si `TRUE`, il est conservé.
#'
#' @return Une matrice de dimensions 1 × 3 contenant, dans l'ordre :
#'   `dimension`, `concept`, `descripteur`. Si `x` est `NA`, retourne
#'   la matrice `c("rien", "rien", "rien")`.
#'
#' @details
#' Le format attendu est `"dimension:concept/descripteur"`, où :
#' \itemize{
#'   \item `":"` sépare la dimension du concept ;
#'   \item `"/"` sépare le concept du descripteur.
#' }
#' Si le descripteur vaut `"_"`, il est remplacé par la valeur du concept.
#'
#' @seealso \code{\link{get.binary}}, \code{\link{data.preprocess.fc}}
#'
#' @examples
#' transfert.MV.BM("texture:croustillant/très_croustillant", tres = FALSE)
#' #>      [,1]      [,2]           [,3]
#' #> [1,] "texture" "croustillant" "croustillant"
#'
#' transfert.MV.BM("texture:croustillant/très_croustillant", tres = TRUE)
#' #>      [,1]      [,2]           [,3]
#' #> [1,] "texture" "croustillant" "très_croustillant"
#'
#' transfert.MV.BM(NA)
#' #>      [,1]    [,2]    [,3]
#' #> [1,] "rien"  "rien"  "rien"
#'
#' @export
#' 

transfert.MV.BM=function(x,tres=FALSE){
  if (is.na(x)){
    retour=matrix(rep("rien",3),1,3)
  }else{
    ou.stop.dimension=gregexpr(":",x)
    dimension=substring(x,1,ou.stop.dimension[[1]][[1]]-1)
    ou.stop.concept=gregexpr("/",x)
    concept=substring(x,ou.stop.dimension[[1]][[1]]+1,ou.stop.concept[[1]][[1]]-1)
    descripteur=substring(x,ou.stop.concept[[1]][[1]]+1)
    if (descripteur=="_"){
      descripteur=concept
    }
    if (!tres){
      descripteur=gsub("_très","",descripteur)
      concept=gsub("_très","",concept)
    }
    retour=matrix(c(dimension,concept,descripteur),1,3)
  }
  return(retour)
}
