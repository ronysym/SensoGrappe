#' @title Pretraitement des donnees pour l'analyse MRCA
#'
#' @description Prepare une matrice binaire (sujets x produits x descripteurs) a partir
#' d'un `data.frame` de commentaires libres deja structure, en vue d'une
#' analyse MRCA (Multiple Response Correspondence Analysis). Inclut la
#' completion des evaluations manquantes et le filtrage des descripteurs
#' peu frequents.
#'
#'
#' @usage
#'preprocess.mrca(
#'  dta,
#'  specific_threshold = 0.05,
#'  specific_coverage = 1)
#'
#'
#' @param dta Un `data.frame` contenant obligatoirement les colonnes
#'   `subject`, `product` et `descripteur`.
#' @param specific_threshold Numerique. Proportion minimale d'observations
#'   par rapport au nombre maximal de sujets par produit (Ep) requise dans
#'   un produit pour qu'un descripteur soit retenu. Par defaut `0.05`
#'   (5%).
#' @param specific_coverage Entier. Nombre minimum de produits devant
#'   satisfaire `specific_threshold` pour qu'un descripteur soit conserve.
#'   Par defaut `1`.
#'
#' @return Un `data.frame` binaire avec les colonnes `subject`, `product`
#'   et une colonne par descripteur retenu (valeurs 0/1), trie par sujet
#'   et produit.
#'
#' @details Les etapes internes sont :
#' \enumerate{
#'   \item Creation d'un tableau de contingence (evaluation x descripteur)
#'     et binarisation (presence/absence).
#'   \item Completion des evaluations manquantes : pour chaque produit,
#'     les sujets absents sont ajoutes avec des vecteurs de zeros. La
#'     reference `Ep` est fixee au maximum observe sur l'ensemble des
#'     produits.
#'   \item Filtrage des descripteurs selon `specific_threshold` et
#'     `specific_coverage`.
#'     }
#' Contrairement a \code{\link{get.binary}}, cette fonction ne recupere
#' pas de niveau hierarchique commun et est dediee a la preparation pour
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
#'#' head(dta_mrca)
#' }
#'
#' @export
preprocess.mrca <- function(
    dta,
    # ====== SEUILS DE FILTRAGE SPECIFIQUE ======
    # Seuil minimum pour la proportion d'observations par rapport a Ep
    # (ex: 0.05 = 5% du nombre maximal de sujets par produit)
    specific_threshold = 0.05,

    # Nombre minimum de produits qui doivent respecter le seuil specifique
    # (0 = au moins un produit, 5 = au moins 5 produits)
    specific_coverage = 1
) {

  # -- Resolution de l'entree : accepte un objet fc.preprocess ou un data.frame --
  if (inherits(dta, "fc.preprocess")) {
    if (is.null(dta$dta)) {
      stop(paste0(
        "L'objet 'fc.preprocess' fourni ne contient pas d'\u00e9l\u00e9ment '$dta'. ",
        "V\u00e9rifiez la sortie de `data.preprocess.fc()`."
      ))
    }
    dta <- dta$dta
  }

  # --- dta ---
  if (missing(dta) || is.null(dta)) {
    stop("L'argument 'dta' est obligatoire et ne peut pas \u00eatre NULL.")
  }
  if (!is.data.frame(dta)) {
    stop("L'argument 'dta' doit \u00eatre un data.frame.")
  }
  if (nrow(dta) == 0) {
    stop("Le data.frame 'dta' est vide (aucune ligne).")
  }

  # --- colonnes obligatoires ---
  required_cols <- c("subject", "product", "descripteur")
  missing_cols  <- setdiff(required_cols, names(dta))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "Colonne(s) manquante(s) dans 'dta' : ",
      paste(missing_cols, collapse = ", "), "."
    ))
  }

  # --- absence de NA dans les colonnes cles ---
  na_counts <- sapply(required_cols, function(col) sum(is.na(dta[[col]])))
  if (any(na_counts > 0)) {
    stop(paste0(
      "Des valeurs manquantes (NA) ont \u00e9t\u00e9 d\u00e9tect\u00e9es dans : ",
      paste(names(na_counts[na_counts > 0]), collapse = ", "), "."
    ))
  }

  # --- absence de chaines vides dans les colonnes cles ---
  empty_counts <- sapply(required_cols, function(col) {
    sum(trimws(as.character(dta[[col]])) == "")
  })
  if (any(empty_counts > 0)) {
    stop(paste0(
      "Des valeurs vides ont \u00e9t\u00e9 d\u00e9tect\u00e9es dans : ",
      paste(names(empty_counts[empty_counts > 0]), collapse = ", "), "."
    ))
  }

  # --- specific_threshold ---
  if (!is.numeric(specific_threshold) || length(specific_threshold) != 1) {
    stop("'specific_threshold' doit \u00eatre une valeur num\u00e9rique scalaire.")
  }
  if (is.na(specific_threshold)) {
    stop("'specific_threshold' ne peut pas \u00eatre NA.")
  }
  if (specific_threshold < 0 || specific_threshold > 1) {
    stop("'specific_threshold' doit \u00eatre compris entre 0 et 1.")
  }

  # --- specific_coverage ---
  if (!is.numeric(specific_coverage) || length(specific_coverage) != 1) {
    stop("'specific_coverage' doit \u00eatre une valeur num\u00e9rique scalaire.")
  }
  if (is.na(specific_coverage)) {
    stop("'specific_coverage' ne peut pas \u00eatre NA.")
  }
  if (specific_coverage < 0 || specific_coverage != round(specific_coverage)) {
    stop("'specific_coverage' doit \u00eatre un entier positif ou nul.")
  }

  # --- au moins 2 produits distincts ---
  n_products <- length(unique(as.character(dta$product)))
  if (n_products < 2) {
    stop(paste0(
      "'dta' doit contenir au moins 2 produits distincts. ",
      n_products, " produit(s) d\u00e9tect\u00e9(s)."
    ))
  }

  # --- au moins 1 sujet ---
  n_subjects <- length(unique(as.character(dta$subject)))
  if (n_subjects < 1) {
    stop("'dta' doit contenir au moins 1 sujet distinct.")
  }

  # --- coherence specific_coverage vs nombre de produits ---
  if (specific_coverage > n_products) {
    stop(paste0(
      "'specific_coverage' (", specific_coverage, ") ne peut pas d\u00e9passer ",
      "le nombre de produits disponibles (", n_products, ")."
    ))
  }

  # --- separateur "et" dans subject ou product (risque de confusion) ---
  if (any(grepl("et", as.character(dta$subject)))) {
    warning(paste0(
      "Certains identifiants 'subject' contiennent la cha\u00eene \"et\", ",
      "utilis\u00e9e comme s\u00e9parateur interne. ",
      "Cela peut provoquer des erreurs lors de la reconstruction des identifiants. ",
      "Envisagez de renommer vos sujets."
    ))
  }
  if (any(grepl("et", as.character(dta$product)))) {
    warning(paste0(
      "Certains identifiants 'product' contiennent la cha\u00eene \"et\", ",
      "utilis\u00e9e comme s\u00e9parateur interne. ",
      "Cela peut provoquer des erreurs lors de la reconstruction des identifiants. ",
      "Envisagez de renommer vos produits."
    ))
  }

  # --- au moins 1 descripteur distinct ---
  n_desc <- length(unique(as.character(dta$descripteur)))
  if (n_desc < 1) {
    stop("'dta' ne contient aucun descripteur distinct.")
  }
  # =============================================
  # ETAPE 1 : PREPARATION DES DONNEES INITIALES
  # =============================================

  # Combinaison des colonnes subject et product pour creer des evaluations uniques
  evalsi <- paste(dta$subject, dta$product, sep = "et")

  # Creation d'un tableau de contingence (produits x descripteurs)
  bin <- table(evalsi, dta$descripteur)
  class(bin) <- "matrix"
  bin[bin > 1] <- 1  # Assurer l'unicit\u00e9 des descripteurs par \u00e9valuation

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

  # Creation du data frame initial
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
  # ETAPE 2 : COMPLETION DES DONNEES MANQUANTES
  # =============================================

  # Calcul du nombre minimal de sujets par produit (Ep)
  get_Ep <- table(dta$subject, dta$product) > 0
  Ep <- colSums(get_Ep)
  Ep <- rep(max(Ep), length(Ep))  # On prend le maximum comme r\u00e9f\u00e9rence

  # Ajout de lignes manquantes pour les produits qui n'ont pas assez d'observations
  if (any(as.numeric(table(retouri$product)) < Ep)) {
    for (pp in levels(retouri$product)) {
      if (sum(retouri$product == pp) < Ep[1]) {
        # Trouver les sujets manquants pour ce produit
        croise <- as.matrix(table(retouri$subject, retouri$product))
        miss_subj <- rownames(croise)[croise[, pp] == 0]

        if (length(miss_subj) > 0) {
          # Creer des lignes manquantes avec des 0 pour les descripteurs
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
  # ETAPE 3 : FILTRAGE FINAL (UNIQUEMENT SPECIFIQUE)
  # =============================================

  # Agregation des donnees par produit
  conti <- aggregate(. ~ product, retouri, sum)
  rownames(conti) <- conti$product
  conti <- conti[, -1, drop = FALSE]  # Supprimer la colonne product

  # === FILTRAGE SPECIFIQUE UNIQUEMENT ===
  # On garde les descripteurs qui apparaissent dans au moins `specific_threshold`
  # proportion des observations attendues (par rapport a Ep) dans au moins
  # `specific_coverage` produits.
  vec_specific <- apply(conti / Ep >= specific_threshold, 2, sum)
  kept_specific <- vec_specific >= specific_coverage

  # === SELECTION FINALE ===
  kept_cols <- colnames(conti)[kept_specific]
  kept_cols <- c("subject", "product", kept_cols)

  # Filtrer le data frame final
  retouri <- retouri[, colnames(retouri) %in% kept_cols, drop = FALSE]

  return(retouri)
}
