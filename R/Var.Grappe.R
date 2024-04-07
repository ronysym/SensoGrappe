#' Var.Grappe : Checking and encoding dataset
#'
#' @description Check dataset and  encode easily the vector (variables) to factor, integer or numeric
#'
#' @param x 	dataframe
#' @param column the numbers of the columns to change the type
#' @param type expected type of the columns to be changed : "factor", "numeric" or "integer". NULL by default
#' @param filter character or numeric corresponding to searching name or number of column
#' @param verbose  logical display of the results. TRUE by default
#'
#' @examples
#' data(wine)
#' Var.Grappe(wine)
#' Var.Grappe(wine, filter = "O_")
#' winef <- Var.Grappe(wine, column = c(1:2), type = "factor")
#'
#' @export
Var.Grappe <- function(x, column = 0, type = NULL, filter = NULL, verbose = TRUE) { # Cree la matrice de resultats

  # Check if x is a dataframe
  if (!is.data.frame(x)) {
    stop("Argument 'x' must be a dataframe.")
  }

  # Check if column is numeric
  if (!is.numeric(column)) {
    stop("Argument 'column' must be numeric.")
  }

  # Check if type is one of "factor", "numeric", or "integer"
  if (!is.null(type) && !type %in% c("factor", "numeric", "integer")) {
    stop("Argument 'type' must be one of 'factor', 'numeric', or 'integer'.")
  }

  # Check if verbose is logical
  if (!is.logical(verbose)) {
    stop("Argument 'verbose' must be logical.")
  }

  # Check if filter is character or numeric
  if (!is.null(filter) && !is.character(filter) && !is.numeric(filter)) {
    stop("Argument 'filter' must be either character or numeric.")
  }

  allvar <- matrix(1:length(names(x)), ncol = 2, nrow = length(names(x)), byrow = FALSE)
  colnames(allvar) <- c("Column", "Type of variable")
  rownames(allvar) <- names(x)
  # Liste des types de variables possibles pour les transformations
  fac <- c("f", "factor")
  num <- c("n", "numeric")
  int <- c("i", "integer")
  #
  if (column[1] != 0 && !is.null(type)) {
    for (i in 1:length(column))
    {
      # Changement du type de variable
      if (length(grep(type, fac, ignore.case = TRUE)) > 0) {
        x[, column[i]] <- as.factor(x[, column[i]])
      }
      if (length(grep(type, num, ignore.case = TRUE)) > 0) {
        x[, column[i]] <- as.integer(x[, column[i]])
      }
      if (length(grep(type, int, ignore.case = TRUE)) > 0) {
        x[, column[i]] <- as.numeric(x[, column[i]])
      }
    }
  }
  # Remplissage de la matrice de resultats avec la column et le type de chaque variable
  for (i in 1:length(names(x)))
  {
    allvar[i, 2] <- class(x[, i])
  }
  # Affiche le resultat
  if (verbose == TRUE) {
    if (is.null(filter)) {
      print(as.data.frame(allvar), right = FALSE)
    } else {
      ligne.temp <- grep(filter, rownames(allvar), ignore.case = TRUE)
      if (length(ligne.temp) == 0) {
        cat("\nNo variable in the dataframe has this name.Check the spelling\n\n")
      } else {
        print(as.data.frame(allvar)[ligne.temp, ], right = FALSE)
      }
    }
  }
  # Retourne le nouveau tableau de donnees avec les nouveaux types de variables
  invisible(as.data.frame(x))
}
