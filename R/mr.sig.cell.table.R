#' Tableau des pourcentages de citations avec coloration selon la significativite
#'
#' @description
#' Genere un tableau HTML interactif (via \pkg{kableExtra}) affichant les
#' pourcentages de citations par modalite, avec une coloration des cellules
#' selon la significativite statistique et la direction (sur- ou
#' sous-representation) par rapport a un profil nul.
#'
#' @param cell.testi Liste issue d'un test de cellules mrCA. Doit contenir les
#'   elements suivants :
#'   \describe{
#'     \item{\code{percent.derived.cont}}{Matrice des pourcentages derives de
#'       la table de contingence (lignes = modalites, colonnes = produits).}
#'     \item{\code{p.value}}{Matrice des p-valeurs associees a chaque cellule.}
#'     \item{\code{original.cont}}{Matrice des effectifs observes.}
#'     \item{\code{null.cont}}{Matrice des effectifs attendus sous H0.}
#'   }
#' @param pvalue.threshold1 Numerique. Seuil de significativite strict.
#'   Les cellules avec p <= \code{pvalue.threshold1} sont colorees avec la
#'   couleur forte (rouge fonce ou bleu fonce). Par defaut \code{0.05}.
#' @param pvalue.threshold2 Numerique. Seuil de significativite modere.
#'   Les cellules avec \code{pvalue.threshold1} < p <= \code{pvalue.threshold2}
#'   sont colorees avec la couleur legere (rouge clair ou bleu clair).
#'   Par defaut \code{0.10}.
#' @param title Chaine de caracteres. Titre du tableau. Par defaut
#'   \code{"Pourcentages des citations par modalite"}.
#'
#' @return Une liste nommee contenant deux elements :
#'   \describe{
#'     \item{\code{table}}{Objet \code{kableExtra} -- tableau principal HTML
#'       avec cellules colorees et defilement vertical.}
#'     \item{\code{legend}}{Objet \code{kableExtra} -- tableau de legende HTML
#'       expliquant le code couleur.}
#'   }
#'
#' @details
#' Le code couleur applique aux cellules est le suivant :
#' \itemize{
#'   \item \strong{Rouge fonce} (\code{#D32F2F}) : sur-representation
#'     significative (p <= \code{pvalue.threshold1}).
#'   \item \strong{Rouge clair} (\code{#FFCDD2}) : sur-representation
#'     tendancielle (\code{pvalue.threshold1} < p <= \code{pvalue.threshold2}).
#'   \item \strong{Bleu fonce} (\code{#1565C0}) : sous-representation
#'     significative (p <= \code{pvalue.threshold1}).
#'   \item \strong{Bleu clair} (\code{#BBDEFB}) : sous-representation
#'     tendancielle (\code{pvalue.threshold1} < p <= \code{pvalue.threshold2}).
#' }
#' La direction (sur/sous) est determinee par la difference entre les effectifs
#' observes (\code{original.cont}) et les effectifs attendus sous H0
#' (\code{null.cont}).
#'
#' @import knitr
#' @import kableExtra
#'
#' @examples
#' \dontrun{
#' # Apres avoir calcule les tests de cellules :
#' cell_results <- mr.cell.test(mrca_object)
#'
#' # Tableau avec seuils par defaut
#' result <- mr.sig.cell.table(cell_results)
#' result$table    # afficher le tableau
#' result$legend   # afficher la legende
#'
#' # Tableau avec seuils personnalises et titre
#' result <- mr.sig.cell.table(cell_results,
#'                              pvalue.threshold1 = 0.01,
#'                              pvalue.threshold2 = 0.05,
#'                              title = "Citations significatives par produit")
#' }
#'
#' @export
mr.sig.cell.table <- function(cell.testi,
                              pvalue.threshold1 = 0.05,
                              pvalue.threshold2 = 0.10,
                              title = "Pourcentages des citations par modalit\u00e9") {

  # Controle de classe
  if (!inherits(cell.testi, "sensory.mr.sig.cell")) {
    stop(
      "L'argument 'cell.testi' doit \u00eatre un objet de classe 'sensory.mr.sig.cell'.\n",
      "  Classe re\u00e7ue : ", paste(class(cell.testi), collapse = ", "), "\n",
      "  Utilisez sensory.mr.sig.cell() pour g\u00e9n\u00e9rer un objet valide.",
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

        kableExtra::cell_spec(val,          # <-- qualifi\u00e9
                              format     = "html",
                              background = bg,
                              color      = txtcol,
                              bold       = bold)
      })
    }, cols, SIMPLIFY = FALSE),
    row.names   = rows,
    check.names = FALSE
  )

  main_table <- knitr::kable(colored_df,   # <-- qualifi\u00e9
                             format  = "html",
                             escape  = FALSE,
                             caption = title,
                             align   = "c") |>
    kableExtra::kable_styling(             # <-- qualifi\u00e9
      bootstrap_options = c("hover", "condensed", "bordered"),
      full_width        = FALSE,
      font_size         = 11,
      fixed_thead       = TRUE
    ) |>
    kableExtra::column_spec(1, bold = TRUE, width = "12em") |>
    kableExtra::scroll_box(width = "100%", height = "600px")

  legend_data <- data.frame(
    Couleur = c("\u25a0", "\u25a0", "\u25a0", "\u25a0"),
    Description = c(
      paste0("Sur-repr\u00e9sent\u00e9  \u2014 p \u2264 ", pvalue.threshold1),
      paste0("Sur-repr\u00e9sent\u00e9  \u2014 ", pvalue.threshold1, " < p \u2264 ", pvalue.threshold2),
      paste0("Sous-repr\u00e9sent\u00e9 \u2014 p \u2264 ", pvalue.threshold1),
      paste0("Sous-repr\u00e9sent\u00e9 \u2014 ", pvalue.threshold1, " < p \u2264 ", pvalue.threshold2)
    ),
    stringsAsFactors = FALSE
  )

  legend_table <- knitr::kable(legend_data,  # <-- qualifi\u00e9
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
