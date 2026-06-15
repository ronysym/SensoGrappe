#' Pretraitement des donnees de commentaires libres
#'
#' @description
#' Fonction principale de pretraitement des donnees issues de l'outil
#' ConsoTextPlorer ou d'un codage manuel. Elle realise la mise en forme,
#' la binarisation et le clustering des descripteurs.
#'
#' @param data Un `data.frame` contenant les donnees brutes a traiter.
#'   Si `manual = FALSE`, les colonnes 1, 2, 3 et 6 sont utilisees
#'   (format ConsoTextPlorer). Si `manual = TRUE`, le data.frame est
#'   utilise tel quel.
#' @param balanced.data Logique. Si `TRUE` (defaut), les donnees sont
#'   equilibrees en completant les evaluations manquantes par des zeros.
#' @param tres Logique. Si `TRUE`, les modificateurs de type `_tres` sont
#'   conserves dans les descripteurs. Par defaut `FALSE`.
#' @param specific_threshold Numerique. Seuil minimum de proportion
#'   d'observations par rapport au nombre attendu de sujets par produit
#'   (Ep) pour qu'un descripteur soit conserve. Par defaut `0.05`.
#' @param specific_coverage Entier. Nombre minimum de produits devant
#'   respecter le seuil `specific_threshold` pour qu'un descripteur soit
#'   conserve. Par defaut `1`.
#' @param manual Logique. Si `FALSE` (defaut), les donnees proviennent de
#'   ConsoTextPlorer et sont transformees via \code{\link{transfert.MV.BM}}.
#'   Si `TRUE`, les donnees sont supposees deja mises en forme.
#' @param common.level Caractere. Niveau hierarchique utilise pour le
#'   regroupement lors du clustering des descripteurs. Doit etre
#'   `"concept"`, `"dimension"` ou `"any"`. Par defaut `"any"`.
#' @param clust.descri Logique. Si `TRUE` (defaut), un clustering des
#'   descripteurs est realise via \code{\link{mr.clust}}.
#'
#' @return Une liste contenant :
#'   \describe{
#'     \item{`dta`}{`data.frame` final avec les colonnes `product`,
#'       `subject` et `descripteur` (et le facteur si present),
#'       trie par sujet et produit.}
#'     \item{`res.algo`}{`data.frame` intermediaire enrichi des colonnes
#'       `dimension`, `concept`, `descripteur` et `dimension.concept`.}
#'     \item{`binary`}{Liste retournee par \code{\link{get.binary}},
#'       contenant la matrice binaire (`dta`) et le vecteur du niveau
#'       commun (`common`).}
#'   }
#'
#' @details
#' Le pipeline complet comprend les etapes suivantes :
#' \enumerate{
#'   \item Mise en forme des donnees brutes (si `manual = FALSE`).
#'   \item Decomposition hierarchique via \code{\link{transfert.MV.BM}}
#'     (dimension / concept / descripteur).
#'   \item Binarisation via \code{\link{get.binary}}.
#'   \item Clustering des descripteurs via \code{\link{mr.clust}}
#'     (si `clust.descri = TRUE`).
#' }
#'
#' Lorsque `common.level = "any"`, aucun regroupement par niveau
#' hierarchique n'est applique avant le clustering : tous les descripteurs
#' sont traites ensemble.
#'
#' @seealso \code{\link{get.binary}}, \code{\link{transfert.MV.BM}},
#'   \code{\link{mr.clust}}, \code{\link{preprocess.mrca}}
#'
#' @examples
#' \dontrun{
#' # Donnees issues de ConsoTextPlorer
#' result <- data.preprocess.fc(
#'   data              = mon_jeu_de_donnees,
#'   balanced.data     = TRUE,
#'   tres              = FALSE,
#'   specific_threshold = 0.05,
#'   specific_coverage  = 1,
#'   manual            = FALSE,
#'   common.level      = "any",
#'   clust.descri      = TRUE
#' )
#'
#' # Acces aux resultats
#' result$dta       # Table finale
#' result$res.algo  # Donnees intermediaires
#' result$binary    # Matrice binaire
#'
#' # Donnees codees manuellement
#' result_manual <- data.preprocess.fc(
#'   data         = mon_jeu_manuel,
#'   manual       = TRUE,
#'   common.level = "concept"
#' )
#' }
#'
#' @importFrom dplyr bind_cols select
#' @export
#'
#'


data.preprocess.fc <- function(
    data,
    balanced.data      = TRUE,
    tres               = FALSE,
    specific_threshold = 0.05,
    specific_coverage  = 1,
    manual             = FALSE,
    common.level       = "any",
    clust.descri       = TRUE
) {

  # -- 0. Validation des arguments ----------------------------------------------

  # -- data ---------------------------------------------------------------------
  if (is.null(data))
    stop("`data` ne peut pas \u00eatre NULL.")
  if (!is.data.frame(data))
    stop("`data` doit \u00eatre un data.frame.")
  if (nrow(data) == 0)
    stop("`data` est vide (0 lignes).")
  if (ncol(data) == 0)
    stop("`data` est vide (0 colonnes).")

  # -- Parametres logiques ------------------------------------------------------
  if (!is.logical(balanced.data) || length(balanced.data) != 1)
    stop("`balanced.data` doit \u00eatre TRUE ou FALSE.")
  if (!is.logical(tres) || length(tres) != 1)
    stop("`tres` doit \u00eatre TRUE ou FALSE.")
  if (!is.logical(manual) || length(manual) != 1)
    stop("`manual` doit \u00eatre TRUE ou FALSE.")
  if (!is.logical(clust.descri) || length(clust.descri) != 1)
    stop("`clust.descri` doit \u00eatre TRUE ou FALSE.")

  # -- specific_threshold -------------------------------------------------------
  if (!is.numeric(specific_threshold) || length(specific_threshold) != 1)
    stop("`specific_threshold` doit \u00eatre un nombre.")
  if (specific_threshold < 0 || specific_threshold > 1)
    stop("`specific_threshold` doit \u00eatre compris entre 0 et 1.")
  if (specific_threshold < 0.01)
    warning("`specific_threshold` est tr\u00e8s bas, risque d'inclure du bruit.")

  # -- specific_coverage --------------------------------------------------------
  if (!is.numeric(specific_coverage) || length(specific_coverage) != 1)
    stop("`specific_coverage` doit \u00eatre un entier.")
  if (specific_coverage < 1)
    stop("`specific_coverage` doit \u00eatre >= 1.")
  if (specific_coverage != as.integer(specific_coverage))
    warning("`specific_coverage` n'est pas un entier, il sera arrondi.")

  # -- common.level -------------------------------------------------------------
  valid.levels <- c("concept", "dimension", "any")
  if (!is.character(common.level) || length(common.level) != 1)
    stop("`common.level` doit \u00eatre une cha\u00eene de caract\u00e8res.")
  if (!common.level %in% valid.levels)
    stop(paste0(
      "`common.level` doit \u00eatre 'concept', 'dimension' ou 'any'. ",
      "Valeur re\u00e7ue : '", common.level, "'."
    ))

  # -- Colonnes requises (mode ConsoTextPlorer) ---------------------------------
  if (!manual) {
    if (ncol(data) < 6)
      stop(paste0(
        "En mode ConsoTextPlorer (`manual = FALSE`), `data` doit ",
        "avoir au moins 6 colonnes. Colonnes d\u00e9tect\u00e9es : ", ncol(data), "."
      ))
  }

  # -- Colonnes requises (mode manuel) ------------------------------------------
  if (manual) {
    cols.requises <- c("product", "subject", "descripteur")
    cols.manquantes <- setdiff(cols.requises, names(data))
    if (length(cols.manquantes) > 0)
      stop(paste0(
        "En mode manuel, les colonnes suivantes sont manquantes : ",
        paste(cols.manquantes, collapse = ", "), "."
      ))
  }

  # -- Coherence common.level + clust.descri + mode manuel ---------------------
  if (manual && clust.descri && common.level %in% c("concept", "dimension")) {
    if (!common.level %in% names(data))
      stop(paste0(
        "La colonne '", common.level, "' est absente de `data`. ",
        "V\u00e9rifiez `common.level` ou utilisez `common.level = 'any'`."
      ))
  }
  # -- 1. Chargement des dependances --------------------------------------------

  if (!manual) {
    # -- 2. Mise en forme initiale ----------------------------------------------
    res.algo <- data %>%
      as.matrix() %>%
      .[, c(1, 2, 3, 6)] %>%
      as.data.frame() %>%
      mutate(factor = substring(factor, 1, 1))

    # -- 3. Application de transfert.MV.BM -------------------------------------
    res.algo <- res.algo %>%
      bind_cols(
        do.call(rbind, lapply(.[, 4], transfert.MV.BM, tres = tres)) %>%
          as.data.frame() %>%
          setNames(c("dimension", "concept", "descripteur"))
      ) %>%
      mutate(dimension.concept = paste(dimension, concept, sep = "."))

  } else {
    res.algo <- data
  }

  # -- 4. Binarisation ----------------------------------------------------------
  # Resolution de common.level = "any" : pas de regroupement par niveau commun
  #common.level.bin <- if (common.level == "any") {
  #  if ("concept" %in% names(res.algo)) "concept" else NULL
 # } else {
  #  common.level
 # }

  binary <- get.binary(
    res.algo,
    common.level       = common.level,
    balanced.data      = balanced.data,
    specific_threshold = specific_threshold,
    specific_coverage  = specific_coverage,
    manual             = manual
  )

  # -- 5. Clustering et fusion des descripteurs ---------------------------------
  if (clust.descri) {

    # Definition des groupes selon common.level
    groupes <- if (common.level == "any") {
      rep("all", length(binary$common))
    } else {
      binary$common
    }

    for (common_val in unique(groupes)) {
      ou.common  <- which(groupes == common_val) + 2
      dta.common <- binary$dta[, c(2, ou.common), drop = FALSE]
      res.common <- mr.clust(dta.common)

      for (cl in unique(res.common$clust)) {
        membres <- names(res.common$clust)[res.common$clust == cl]
        target  <- paste(membres, collapse = "/")
        for (desc in membres) {
          res.algo$descripteur[res.algo$descripteur == desc] <- target
        }
      }
    }
  }

  # -- 6. Table finale ----------------------------------------------------------
  dta <- if ("factor" %in% names(res.algo)) {
    res.algo %>%
      mutate(descripteur = paste(factor, descripteur, sep = ".")) %>%
      arrange(subject, product) %>%
      select(product, subject, descripteur)
  } else {
    res.algo %>%
      arrange(subject, product) %>%
      select(product, subject, descripteur)
  }

# -- 7. Retour ----------------------------------------------------------------
  result <- list(
    dta      = dta,
    res.algo = res.algo,
    binary   = binary
  )

  # Attribution de la classe S3
  class(result) <- c("fc.preprocess", "list")

  return(result)
}
