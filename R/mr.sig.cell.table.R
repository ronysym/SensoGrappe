#' Tableau des pourcentages de citations avec coloration selon la significativité
#'
#' @description
#' Génère un tableau HTML interactif (via \pkg{kableExtra}) affichant les
#' pourcentages de citations par modalité, avec une coloration des cellules
#' selon la significativité statistique et la direction (sur- ou
#' sous-représentation) par rapport à un profil nul.
#'
#' @param cell.testi Liste issue d'un test de cellules mrCA. Doit contenir les
#'   éléments suivants :
#'   \describe{
#'     \item{\code{percent.derived.cont}}{Matrice des pourcentages dérivés de
#'       la table de contingence (lignes = modalités, colonnes = produits).}
#'     \item{\code{p.value}}{Matrice des p-valeurs associées à chaque cellule.}
#'     \item{\code{original.cont}}{Matrice des effectifs observés.}
#'     \item{\code{null.cont}}{Matrice des effectifs attendus sous H0.}
#'   }
#' @param pvalue.threshold1 Numérique. Seuil de significativité strict.
#'   Les cellules avec p ≤ \code{pvalue.threshold1} sont colorées avec la
#'   couleur forte (rouge foncé ou bleu foncé). Par défaut \code{0.05}.
#' @param pvalue.threshold2 Numérique. Seuil de significativité modéré.
#'   Les cellules avec \code{pvalue.threshold1} < p ≤ \code{pvalue.threshold2}
#'   sont colorées avec la couleur légère (rouge clair ou bleu clair).
#'   Par défaut \code{0.10}.
#' @param title Chaîne de caractères. Titre du tableau. Par défaut
#'   \code{"Pourcentages des citations par modalité"}.
#'
#' @return Une liste nommée contenant deux éléments :
#'   \describe{
#'     \item{\code{table}}{Objet \code{kableExtra} — tableau principal HTML
#'       avec cellules colorées et défilement vertical.}
#'     \item{\code{legend}}{Objet \code{kableExtra} — tableau de légende HTML
#'       expliquant le code couleur.}
#'   }
#'
#' @details
#' Le code couleur appliqué aux cellules est le suivant :
#' \itemize{
#'   \item \strong{Rouge foncé} (\code{#D32F2F}) : sur-représentation
#'     significative (p ≤ \code{pvalue.threshold1}).
#'   \item \strong{Rouge clair} (\code{#FFCDD2}) : sur-représentation
#'     tendancielle (\code{pvalue.threshold1} < p ≤ \code{pvalue.threshold2}).
#'   \item \strong{Bleu foncé} (\code{#1565C0}) : sous-représentation
#'     significative (p ≤ \code{pvalue.threshold1}).
#'   \item \strong{Bleu clair} (\code{#BBDEFB}) : sous-représentation
#'     tendancielle (\code{pvalue.threshold1} < p ≤ \code{pvalue.threshold2}).
#' }
#' La direction (sur/sous) est déterminée par la différence entre les effectifs
#' observés (\code{original.cont}) et les effectifs attendus sous H0
#' (\code{null.cont}).
#'
#' @import knitr
#' @import kableExtra
#'
#' @examples
#' \dontrun{
#' # Après avoir calculé les tests de cellules :
#' cell_results <- mr.cell.test(mrca_object)
#'
#' # Tableau avec seuils par défaut
#' result <- mr.sig.cell.table(cell_results)
#' result$table    # afficher le tableau
#' result$legend   # afficher la légende
#'
#' # Tableau avec seuils personnalisés et titre
#' result <- mr.sig.cell.table(cell_results,
#'                              pvalue.threshold1 = 0.01,
#'                              pvalue.threshold2 = 0.05,
#'                              title = "Citations significatives par produit")
#' }

mr.sig.cell.table <- function(cell.testi,
                              pvalue.threshold1 = 0.05,
                              pvalue.threshold2 = 0.10,
                              title = "Pourcentages des citations par modalité") {

  # Contrôle de classe
  if (!inherits(cell.testi, "sensory.mr.sig.cell")) {
    stop(
      "L'argument 'cell.testi' doit être un objet de classe 'sensory.mr.sig.cell'.\n",
      "  Classe reçue : ", paste(class(cell.testi), collapse = ", "), "\n",
      "  Utilisez sensory.mr.sig.cell() pour générer un objet valide.",
      call. = FALSE
    )
  }



  percent   <- as.data.frame(cell.testi$percent.derived.cont)
  pvalues   <- as.data.frame(cell.testi$p.value)
  original  <- as.data.frame(cell.testi$original.cont)
  null_cont <- as.data.frame(cell.testi$null.cont)

  direction <- original - null_cont

  col_red_strong  <- "#D32F2F"
  col_red_light   <- "#FFCDD2"
  col_blue_strong <- "#1565C0"
  col_blue_light  <- "#BBDEFB"

  rows <- rownames(percent)
  cols <- colnames(percent)

  colored_df <- as.data.frame(
    mapply(function(col_name) {
      sapply(rows, function(row_name) {
        val    <- format(round(percent[row_name, col_name], 1), nsmall = 1)
        pval   <- pvalues[row_name, col_name]
        dir    <- direction[row_name, col_name]

        bg     <- "white"
        txtcol <- "black"
        bold   <- FALSE

        if (!is.na(pval) && !is.na(dir)) {
          if (dir > 0) {
            if (pval <= pvalue.threshold1) {
              bg <- col_red_strong; txtcol <- "white"; bold <- TRUE
            } else if (pval <= pvalue.threshold2) {
              bg <- col_red_light;  txtcol <- "black"; bold <- FALSE
            }
          } else if (dir < 0) {
            if (pval <= pvalue.threshold1) {
              bg <- col_blue_strong; txtcol <- "white"; bold <- TRUE
            } else if (pval <= pvalue.threshold2) {
              bg <- col_blue_light;  txtcol <- "black"; bold <- FALSE
            }
          }
        }

        kableExtra::cell_spec(val,          # <-- qualifié
                              format     = "html",
                              background = bg,
                              color      = txtcol,
                              bold       = bold)
      })
    }, cols, SIMPLIFY = FALSE),
    row.names   = rows,
    check.names = FALSE
  )

  main_table <- knitr::kable(colored_df,   # <-- qualifié
                             format  = "html",
                             escape  = FALSE,
                             caption = title,
                             align   = "c") |>
    kableExtra::kable_styling(             # <-- qualifié
      bootstrap_options = c("hover", "condensed", "bordered"),
      full_width        = FALSE,
      font_size         = 11,
      fixed_thead       = TRUE
    ) |>
    kableExtra::column_spec(1, bold = TRUE, width = "12em") |>
    kableExtra::scroll_box(width = "100%", height = "600px")

  legend_data <- data.frame(
    Couleur = c("■", "■", "■", "■"),
    Description = c(
      paste0("Sur-représenté  — p ≤ ", pvalue.threshold1),
      paste0("Sur-représenté  — ", pvalue.threshold1, " < p ≤ ", pvalue.threshold2),
      paste0("Sous-représenté — p ≤ ", pvalue.threshold1),
      paste0("Sous-représenté — ", pvalue.threshold1, " < p ≤ ", pvalue.threshold2)
    ),
    stringsAsFactors = FALSE
  )

  legend_table <- knitr::kable(legend_data,  # <-- qualifié
                               format    = "html",
                               escape    = FALSE,
                               align     = c("c", "l"),
                               col.names = c("", "Signification")) |>
    kableExtra::kable_styling(
      bootstrap_options = c("bordered"),
      full_width        = FALSE,
      font_size         = 11
    ) |>
    kableExtra::row_spec(1, background = col_red_strong,  color = "white", bold = TRUE) |>
    kableExtra::row_spec(2, background = col_red_light,   color = "black") |>
    kableExtra::row_spec(3, background = col_blue_strong, color = "white", bold = TRUE) |>
    kableExtra::row_spec(4, background = col_blue_light,  color = "black")

  return(list(table  = main_table,
              legend = legend_table))
}
