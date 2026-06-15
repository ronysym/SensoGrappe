#' Binarisation des donnees de commentaires libres
#'
#' @description
#' Construit une matrice binaire (sujets x produits x descripteurs) a
#' partir des donnees de commentaires libres, avec completion des
#' evaluations manquantes et filtrage des descripteurs peu frequents.
#'
#' @param res.algo Un `data.frame` contenant au minimum les colonnes
#'   `subject`, `product` et `descripteur`. Peut egalement contenir
#'   `dimension`, `concept` et `dimension.concept` selon le flux de
#'   traitement.
#' @param common.level Caractere. Niveau hierarchique a recuperer pour
#'   chaque descripteur conserve. Doit etre `"concept"`, `"dimension"`
#'   ou `"any"`. Par defaut `"concept"`.
#' @param balanced.data Logique. Si `TRUE` (defaut), les donnees sont
#'   equilibrees : les evaluations manquantes sont completees par des
#'   lignes de zeros.
#' @param manual Logique. Si `TRUE`, indique que les donnees proviennent
#'   d'un codage manuel. Influence la recuperation du niveau commun.
#'   Par defaut `FALSE`.
#' @param specific_threshold Numerique. Proportion minimale d'observations
#'   par rapport a Ep requise dans un produit pour qu'un descripteur soit
#'   considere comme present dans ce produit. Par defaut `0.05`.
#' @param specific_coverage Entier. Nombre minimum de produits devant
#'   satisfaire `specific_threshold` pour qu'un descripteur soit conserve.
#'   Par defaut `1`.
#'
#' @return Une liste contenant :
#'   \describe{
#'     \item{`dta`}{`data.frame` binaire avec les colonnes `subject`,
#'       `product` et une colonne par descripteur conserve (valeurs 0/1).}
#'     \item{`common`}{Vecteur nomme donnant le niveau commun
#'       (selon `common.level`) de chaque descripteur conserve.
#'       Vaut `"idem"` pour tous les descripteurs si le niveau n'est
#'       pas disponible.}
#'   }
#'
#' @details
#' Les etapes internes sont :
#' \enumerate{
#'   \item Creation d'un tableau de contingence (evaluation x descripteur)
#'     et binarisation (presence/absence).
#'   \item Completion des evaluations manquantes (si `balanced.data = TRUE`).
#'   \item Filtrage des descripteurs selon `specific_threshold` et
#'     `specific_coverage`.
#'   \item Recuperation du niveau hierarchique commun pour chaque
#'     descripteur retenu.
#' }
#'
#' Le parametre `Ep` correspond au nombre de sujets attendus par produit :
#' en mode equilibre (`balanced.data = TRUE`), il est fixe au nombre total
#' de sujets ; sinon, il reflete les observations reelles.
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
  # ETAPE 1 : PREPARATION DES DONNEES INITIALES
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
  # ETAPE 2 : COMPLETION DES DONNEES MANQUANTES
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
  # ETAPE 3 : FILTRAGE FINAL
  # =============================================

  cont <- aggregate(. ~ product, retour.1, sum)
  rownames(cont) <- cont$product
  cont <- cont[, -c(1:2)]

  vec_specific <- apply(cont / Ep >= specific_threshold, 2, sum)
  kept_specific <- vec_specific >= specific_coverage

  kept <- colnames(cont)[kept_specific]

  if (length(kept) == 0) {
    warning("Aucun descripteur conserv\u00e9 avec ces crit\u00e8res !")
  }

  kept <- c("subject", "product", kept)
  retour.1 <- retour.1[, colnames(retour.1) %in% kept]

  # =============================================
  # ETAPE 4 : RECUPERATION DU NIVEAU COMMUN
  # =============================================


  vec <- colnames(retour.1)[-c(1:2)]

  if (!manual | common.level %in% names(res.algo)) {
    # Recuperation du niveau commun pour chaque descripteur
    get.common <- function(mot) {
      res.algo[match(mot, res.algo$descripteur), common.level]
    }
    retour.2 <- unlist(sapply(vec, get.common))
    names(retour.2) <- as.character(seq_along(retour.2))

  } else {
    # Niveau commun non disponible : valeur par defaut
    retour.2 <- rep("idem", length(vec))
    names(retour.2) <- as.character(seq_along(retour.2))
  }

  return(list(dta = retour.1, common = retour.2))
}


##################################################
#' Decomposition d'un descripteur hierarchique (format ConsoTextPlorer)
#'
#' @description
#' Decompose une chaine de caracteres au format ConsoTextPlorer
#' (`"dimension:concept/descripteur"`) en ses trois composantes :
#' dimension, concept et descripteur.
#'
#' @param x Caractere. Chaine a decomposer, au format
#'   `"dimension:concept/descripteur"`. Peut etre `NA`.
#' @param tres Logique. Si `FALSE` (defaut), le suffixe `_tres` est
#'   supprime des composantes `concept` et `descripteur`.
#'   Si `TRUE`, il est conserve.
#'
#' @return Une matrice de dimensions 1 x 3 contenant, dans l'ordre :
#'   `dimension`, `concept`, `descripteur`. Si `x` est `NA`, retourne
#'   la matrice `c("rien", "rien", "rien")`.
#'
#' @details
#' Le format attendu est `"dimension:concept/descripteur"`, ou :
#' \itemize{
#'   \item `":"` separe la dimension du concept ;
#'   \item `"/"` separe le concept du descripteur.
#' }
#' Si le descripteur vaut `"_"`, il est remplace par la valeur du concept.
#'
#' @seealso \code{\link{get.binary}}, \code{\link{data.preprocess.fc}}
#'
#' @examples
#' transfert.MV.BM("texture:croustillant/tres_croustillant", tres = FALSE)
#' #>      [,1]      [,2]           [,3]
#' #> [1,] "texture" "croustillant" "croustillant"
#'
#' transfert.MV.BM("texture:croustillant/tres_croustillant", tres = TRUE)
#' #>      [,1]      [,2]           [,3]
#' #> [1,] "texture" "croustillant" "tres_croustillant"
#'
#' transfert.MV.BM(NA)
#' #>      [,1]    [,2]    [,3]
#' #> [1,] "rien"  "rien"  "rien"
#'
#' @export
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
      descripteur=gsub("_tr\u00e8s","",descripteur)
      concept=gsub("_tr\u00e8s","",concept)
    }
    retour=matrix(c(dimension,concept,descripteur),1,3)
  }
  return(retour)
}
