#' Free comment data (not manually annotated)
#'
#' @description
#' A dataset of free comments collected from ConsoTextPlorer without manual
#' annotation. Contains raw textual responses linked to subjects, products,
#' and semantic dimensions.
#'
#' @format A tibble with 9345 rows and 6 variables:
#' \describe{
#'   \item{subject}{Character. Identifier of the evaluating subject.}
#'   \item{product}{Character. Identifier of the evaluated product.}
#'   \item{factor}{Character. Single-letter factor code (e.g. "D", "Q").}
#'   \item{contexte/descripteur/quantifieur}{Character. Raw contextual
#'     descriptor/qualifier string from ConsoTextPlorer.}
#'   \item{dimension:concept_intensite/descripteur_intensite}{Character.
#'     Hierarchical semantic annotation (dimension, concept, descriptor).}
#'   \item{dimension:concept_intensite/descripteur_intensite (sans bien/mal)}{
#'     Character. Same annotation with "bien"/"mal" modifiers removed.}
#' }
#'
#' @source Generated from ConsoTextPlorer outputs in the GRAPPE laboratory.
"data_fc_not_manual"
