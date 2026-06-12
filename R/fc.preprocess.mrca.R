#' Prétraitement des données pour l'analyse MRCA
#'
#' @description
#' Prépare une matrice binaire (sujets × produits × descripteurs) à partir
#' d'un `data.frame` de commentaires libres déjà structuré, en vue d'une
#' analyse MRCA (Multiple Response Correspondence Analysis). Inclut la
#' complétion des évaluations manquantes et le filtrage des descripteurs
#' peu fréquents.
#'
#' @param dta Un `data.frame` contenant obligatoirement les colonnes
#'   `subject`, `product` et `descripteur`.
#' @param specific_threshold Numérique. Proportion minimale d'observations
#'   par rapport au nombre maximal de sujets par produit (Ep) requise dans
#'   un produit pour qu'un descripteur soit retenu. Par défaut `0.05`
#'   (5 \%).
#' @param specific_coverage Entier. Nombre minimum de produits devant
#'   satisfaire `specific_threshold` pour qu'un descripteur soit conservé.
#'   Par défaut `1`.
#'
#' @return Un `data.frame` binaire avec les colonnes `subject`, `product`
#'   et une colonne par descripteur retenu (valeurs 0/1), trié par sujet
#'   et produit.
#'
#' @details
#' Les étapes internes sont :
#' \enumerate{
#'   \item Création d'un tableau de contingence (évaluation × descripteur)
#'     et binarisation (présence/absence).
#'   \item Complétion des évaluations manquantes : pour chaque produit,
#'     les sujets absents sont ajoutés avec des vecteurs de zéros. La
#'     référence `Ep` est fixée au maximum observé sur l'ensemble des
#'     produits.
#'   \item Filtrage des descripteurs selon `specific_threshold` et
#'     `specific_coverage`.
#' }
#'
#' Contrairement à \code{\link{get.binary}}, cette fonction ne récupère
#' pas de niveau hiérarchique commun et est dédiée à la préparation pour
#' l'analyse MRCA.
#'
#' @seealso \code{\link{data.preprocess.fc}}, \code{\link{get.binary}}
#'
#' @examples
#' \dontrun{
#' dta_mrca <- preprocess.mrca(
#'   dta                = mon_jeu_structure,
#'   specific_threshold = 0.05,
#'   specific_coverage  = 1
#' )
#'
#' head(dta_mrca)
#' }
#'
#' @export


preprocess.mrca <- function(
    dta,
    # ====== SEUILS DE FILTRAGE SPÉCIFIQUE ======
    # Seuil minimum pour la proportion d'observations par rapport à Ep
    # (ex: 0.05 = 5% du nombre maximal de sujets par produit)
    specific_threshold = 0.05,
    
    # Nombre minimum de produits qui doivent respecter le seuil spécifique
    # (0 = au moins un produit, 5 = au moins 5 produits)
    specific_coverage = 1
) {
  # Vérifier que les colonnes nécessaires existent
  required_cols <- c("subject", "product", "descripteur")
  if (!all(required_cols %in% names(dta))) {
    stop("Le data frame doit contenir les colonnes : subject, product, descripteur")
  }
  
  # =============================================
  # ÉTAPE 1 : PRÉPARATION DES DONNÉES INITIALES
  # =============================================
  
  # Combinaison des colonnes subject et product pour créer des évaluations uniques
  evalsi <- paste(dta$subject, dta$product, sep = "et")
  
  # Création d'un tableau de contingence (produits × descripteurs)
  bin <- table(evalsi, dta$descripteur)
  class(bin) <- "matrix"
  bin[bin > 1] <- 1  # Assurer l'unicité des descripteurs par évaluation
  
  # Extraction des sujets et produits des noms de lignes (format "subjectetproduct")
  lsplit <- gregexpr("et", rownames(bin))
  get_subject <- function(idx) {
    ls <- lsplit[[idx]]
    substring(rownames(bin)[idx], 1, ls[[1]] - 1)
  }
  get_product <- function(idx) {
    ls <- lsplit[[idx]]
    substring(rownames(bin)[idx], ls[[1]] + 2)
  }
  
  # Création du data frame initial
  n_rows <- nrow(bin)
  retouri <- data.frame(
    subject = sapply(1:n_rows, get_subject),
    product = sapply(1:n_rows, get_product),
    stringsAsFactors = FALSE
  )
  retouri <- cbind(retouri, as.data.frame.matrix(bin))
  rownames(retouri) <- as.character(1:nrow(retouri))
  retouri$subject <- as.factor(retouri$subject)
  retouri$product <- as.factor(retouri$product)
  retouri <- retouri[order(retouri$subject, retouri$product), ]
  
  # =============================================
  # ÉTAPE 2 : COMPLÉTION DES DONNÉES MANQUANTES
  # =============================================
  
  # Calcul du nombre minimal de sujets par produit (Ep)
  get_Ep <- table(dta$subject, dta$product) > 0
  Ep <- colSums(get_Ep)
  Ep <- rep(max(Ep), length(Ep))  # On prend le maximum comme référence
  
  # Ajout de lignes manquantes pour les produits qui n'ont pas assez d'observations
  if (any(as.numeric(table(retouri$product)) < Ep)) {
    for (pp in levels(retouri$product)) {
      if (sum(retouri$product == pp) < Ep[1]) {
        # Trouver les sujets manquants pour ce produit
        croise <- as.matrix(table(retouri$subject, retouri$product))
        miss_subj <- rownames(croise)[croise[, pp] == 0]
        
        if (length(miss_subj) > 0) {
          # Créer des lignes manquantes avec des 0 pour les descripteurs
          n_missing <- length(miss_subj)
          descripteur_cols <- setdiff(colnames(retouri), c("subject", "product"))
          ajout <- data.frame(
            subject = miss_subj,
            product = rep(pp, n_missing),
            matrix(0, nrow = n_missing, ncol = length(descripteur_cols),
                   dimnames = list(NULL, descripteur_cols)),
            stringsAsFactors = FALSE
          )
          colnames(ajout) <- colnames(retouri)
          retouri <- rbind.data.frame(retouri, ajout)
          retouri <- retouri[order(retouri$subject, retouri$product), ]
        }
      }
    }
  }
  
  # =============================================
  # ÉTAPE 3 : FILTRAGE FINAL (UNIQUEMENT SPÉCIFIQUE)
  # =============================================
  
  # Agrégation des données par produit
  conti <- aggregate(. ~ product, retouri, sum)
  rownames(conti) <- conti$product
  conti <- conti[, -1, drop = FALSE]  # Supprimer la colonne product
  
  # === FILTRAGE SPÉCIFIQUE UNIQUEMENT ===
  # On garde les descripteurs qui apparaissent dans au moins `specific_threshold`
  # proportion des observations attendues (par rapport à Ep) dans au moins
  # `specific_coverage` produits.
  vec_specific <- apply(conti / Ep >= specific_threshold, 2, sum)
  kept_specific <- vec_specific >= specific_coverage
  
  # === SÉLECTION FINALE ===
  kept_cols <- colnames(conti)[kept_specific]
  kept_cols <- c("subject", "product", kept_cols)
  
  # Filtrer le data frame final
  retouri <- retouri[, colnames(retouri) %in% kept_cols, drop = FALSE]
  
  return(retouri)
}
