#' Plot method for sensory mrCA objects
#'
#' @description
#' Génère un graphique ggplot2 pour un objet de classe \code{sensory.mrCA},
#' incluant les coordonnées produits, les flèches descripteurs, les ellipses
#' de bootstrap et les segments de non-différence entre produits.
#'
#' @param x Un objet de classe \code{sensory.mrCA}.
#' @param axes Vecteur numérique de longueur 2 indiquant les dimensions à
#'   représenter. Par défaut \code{c(1, 2)}.
#' @param ellipse Logique. Si \code{TRUE}, affiche les ellipses de confiance
#'   bootstrap autour des produits. Par défaut \code{TRUE}.
#' @param alpha.total.bootstrap.test Seuil de significativité pour les tests
#'   de non-différence entre produits (segments). Par défaut \code{0.05}.
#' @param alpha.ellipse Seuil utilisé pour le calcul des ellipses de confiance
#'   bootstrap. Par défaut \code{0.05}.
#' @param select.desc Vecteur de caractères indiquant les descripteurs à
#'   afficher. Par défaut \code{rownames(x$desc.coord)} (tous les descripteurs).
#' @param rev.x Logique. Si \code{TRUE}, inverse l'axe horizontal. Par défaut
#'   \code{FALSE}.
#' @param rev.y Logique. Si \code{TRUE}, inverse l'axe vertical. Par défaut
#'   \code{FALSE}.
#' @param size.points Numérique. Taille des points produits (multiplicateur).
#'   Par défaut \code{1}.
#' @param size.lab Numérique. Taille des étiquettes (multiplicateur). Par
#'   défaut \code{0.6}.
#' @param size.head.arrow Numérique. Taille de la tête des flèches en cm.
#'   \code{0} supprime les têtes de flèches. Par défaut \code{0}.
#' @param expansion Numérique. Facteur d'expansion des coordonnées descripteurs
#'   relativement aux coordonnées produits. Par défaut \code{1.25}.
#' @param title Chaîne de caractères. Titre du graphique. Si \code{NULL},
#'   le titre par défaut \code{"sensory mrCA graph"} est utilisé.
#' @param col.prod Couleur des points produits. Par défaut \code{"blue"}.
#' @param col.desc Couleur des flèches et étiquettes des descripteurs actifs.
#'   Par défaut \code{"black"}.
#' @param col.proj Couleur des flèches et étiquettes des descripteurs
#'   supplémentaires (projetés). Par défaut \code{"red4"}.
#' @param col.ellipse Couleur des ellipses de bootstrap. Par défaut
#'   \code{"blue"}.
#' @param col.segment Couleur des segments reliant les produits non
#'   significativement différents. Par défaut \code{"blue"}.
#' @param xlim Vecteur numérique de longueur 2 définissant les limites de
#'   l'axe horizontal. Si \code{NULL}, calculé automatiquement.
#' @param ylim Vecteur numérique de longueur 2 définissant les limites de
#'   l'axe vertical. Si \code{NULL}, calculé automatiquement.
#' @param label Caractère indiquant quels éléments étiqueter. Valeurs
#'   possibles : \code{"all"}, \code{"none"}, \code{"prod"}, \code{"desc"}.
#'   Par défaut \code{"all"}.
#' @param invisible Caractère indiquant quels éléments masquer. Valeurs
#'   possibles : \code{"none"}, \code{"prod"}, \code{"desc"}.
#'   Par défaut \code{"none"}.
#' @param autoLab Caractère contrôlant le placement automatique des étiquettes
#'   via \pkg{ggrepel}. Valeurs possibles : \code{"auto"} (automatique selon
#'   le nombre d'éléments), \code{"yes"}, \code{"no"}.
#'   Par défaut \code{"auto"}.
#' @param select Non utilisé actuellement. Réservé pour une sélection future
#'   d'individus. Par défaut \code{NULL}.
#' @param unselect Numérique. Transparence des éléments non sélectionnés
#'   (entre 0 et 1). Par défaut \code{0.7}.
#' @param palette Vecteur de couleurs utilisé pour l'habillage. Si \code{NULL},
#'   une palette interne de 36 couleurs est utilisée.
#' @param habillage Caractère. Si différent de \code{"none"}, colore les
#'   produits selon un groupe. Par défaut \code{"none"}.
#' @param col.hab Vecteur de couleurs pour l'habillage. Si \code{NULL}, la
#'   palette est utilisée automatiquement.
#' @param legend Liste de paramètres de légende. Supporte les entrées
#'   \code{bty}, \code{x} (position) et \code{title}. Par défaut
#'   \code{list(bty = "y", x = "topleft")}.
#' @param ggoptions Liste nommée d'options graphiques avancées. Les entrées
#'   reconnues sont : \code{size}, \code{point.shape}, \code{line.lty},
#'   \code{line.lwd}, \code{line.color}, \code{segment.lty},
#'   \code{segment.lwd}, \code{circle.lty}, \code{circle.lwd},
#'   \code{circle.color}, \code{low.col.quanti}, \code{high.col.quanti}.
#'   Par défaut \code{NULL}.
#' @param new.plot Logique. Si \code{TRUE} et hors RStudio, ouvre une nouvelle
#'   fenêtre graphique. Par défaut \code{FALSE}.
#' @param ... Arguments supplémentaires. Supporte notamment \code{cex},
#'   \code{cex.axis} et \code{cex.main} pour la compatibilité avec les
#'   fonctions graphiques de base R.
#'
#' @return Un objet \code{ggplot} représentant la carte sensorielle mrCA.
#'
#' @details
#' Les ellipses de confiance sont calculées à partir des réplicats bootstrap
#' stockés dans \code{x$bootstrap.replicate.coord}, en utilisant la distance
#' de Mahalanobis et le quantile empirique au niveau \code{1 - alpha.ellipse}.
#'
#' Les segments relient les paires de produits dont la p-valeur du test
#' bootstrap global (\code{x$total.bootstrap.test.pvalues}) est supérieure
#' au seuil \code{alpha.total.bootstrap.test}, indiquant une absence de
#' différence significative.
#'
#' Les coordonnées des descripteurs sont mises à l'échelle par un facteur
#' d'expansion basé sur le rapport entre la norme maximale des produits et
#' celle des descripteurs, multiplié par \code{expansion}.
#'
#' @seealso
#' \code{\link[ellipse]{ellipse}}, \code{\link[ggrepel]{geom_text_repel}},
#' \code{\link[ggrepel]{geom_label_repel}}
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_text geom_segment geom_path
#'   geom_hline geom_vline coord_fixed theme_light theme element_text
#'   element_blank xlab ylab ggtitle xlim ylim scale_color_manual labs arrow
#'   unit
#' @importFrom ggrepel geom_text_repel geom_label_repel
#' @importFrom ellipse ellipse
#'
#' @examples
#' \dontrun{
#' # Après avoir réalisé une analyse mrCA :
#' result_mrca <- sensory.mrCA(data = my_data, nbaxes.sig = 3)
#'
#' # Graphique de base sur les deux premières dimensions
#' plot.sensory.mrCA.grappe(result_mrca)
#'
#' # Dimensions 1 et 3, sans ellipses, avec inversion de l'axe x
#' plot.sensory.mrCA.grappe(result_mrca,
#'                          axes    = c(1, 3),
#'                          ellipse = FALSE,
#'                          rev.x   = TRUE)
#'
#' # Habillage des produits avec couleurs personnalisées
#' plot.sensory.mrCA.grappe(result_mrca,
#'                          habillage = "prod",
#'                          col.hab   = c("red", "blue", "green"))
#'
#' # Sélection de descripteurs spécifiques
#' plot.sensory.mrCA.grappe(result_mrca,
#'                          select.desc = c("Fruité", "Sucré", "Acide"))
#' }
#'
#' @export
sensory.plot.mrCA.grappe <- function(x,
                                     axes = c(1, 2),
                                     ellipse = TRUE,
                                     alpha.total.bootstrap.test = 0.05,
                                     alpha.ellipse = 0.05,
                                     select.desc = rownames(x$desc.coord),
                                     rev.x = FALSE,
                                     rev.y = FALSE,
                                     size.points = 1,
                                     size.lab = 0.6,
                                     size.head.arrow = 0,
                                     expansion = 1.25,
                                     title = NULL,
                                     col.prod = "blue",
                                     col.desc = "black",
                                     col.proj = "red4",
                                     col.ellipse = "blue",
                                     col.segment = "blue",
                                     xlim = NULL,
                                     ylim = NULL,
                                     label = c("all", "none", "prod", "desc"),
                                     invisible = c("none", "prod", "desc"),
                                     autoLab = c("auto", "yes", "no"),
                                     select = NULL,
                                     unselect = 0.7,
                                     palette = NULL,
                                     habillage = "none",
                                     col.hab = NULL,
                                     legend = list(bty = "y", x = "topleft"),
                                     ggoptions = NULL,
                                     new.plot = FALSE,
                                     ...) {

  # ---- Vérifications ----
  if (!inherits(x, "sensory.mrCA")) {
    stop("class(x) must be sensory.mrCA")
  }

  check.axes <- ncol(x$bootstrap.replicate.coord) - 1
  if (max(axes) > check.axes) {
    stop("max(axes) must be lower than or equal to the number of nbaxes.sig used in the mrCA")
  }

  classe <- class(select.desc)
  if (classe != "character" & !is.null(select.desc)) {
    stop("class(select.desc) must be character or NULL")
  }

  # ---- Arguments supplémentaires ----
  argument <- list(...)
  if (!is.null(argument[["cex"]]) & is.null(ggoptions["size"])) ggoptions["size"] <- 4 * argument$cex

  ggoptions_default <- list(
    size            = 4,
    point.shape     = 19,
    line.lty        = 2,
    line.lwd        = 0.5,
    line.color      = "grey",
    segment.lty     = 1,
    segment.lwd     = 0.5,
    circle.lty      = 1,
    circle.lwd      = 0.5,
    circle.color    = "blue",
    low.col.quanti  = "blue",
    high.col.quanti = "red3"
  )
  if (!is.null(ggoptions[1])) ggoptions_default[names(ggoptions)] <- ggoptions[names(ggoptions)]

  # ---- Palette ----
  old.palette <- palette()
  if (is.null(palette)) palette <- c("black", "red", "green3", "blue", "magenta",
                                     "darkgoldenrod", "darkgray", "orange", "cyan", "violet",
                                     "lightpink", "lavender", "yellow", "darkgreen", "turquoise",
                                     "lightgrey", "lightblue", "darkkhaki", "darkmagenta", "lightgreen",
                                     "darkolivegreen", "lightcyan", "darkorange", "darkorchid",
                                     "darkred", "darksalmon", "darkseagreen", "darkslateblue",
                                     "darkslategray", "darkslategrey", "darkturquoise", "darkviolet",
                                     "lightgray", "lightsalmon", "lightyellow", "maroon")
  palette(palette)

  # ---- Labels ----
  label     <- match.arg(label,     c("all", "none", "prod", "desc"), several.ok = TRUE)
  invisible <- match.arg(invisible, c("none", "prod", "desc"),        several.ok = TRUE)
  if ("none" %in% invisible) invisible <- NULL

  lab.prod <- lab.desc <- FALSE
  if (length(label) == 1 && label == "all") lab.prod <- lab.desc <- TRUE
  if ("prod" %in% label) lab.prod <- TRUE
  if ("desc" %in% label) lab.desc <- TRUE

  # ---- autoLab ----
  autoLab <- match.arg(autoLab, c("auto", "yes", "no"))
  if (autoLab == "yes") autoLab <- TRUE
  if (autoLab == "no")  autoLab <- FALSE

  # ---- Labels des axes ----
  lab.x <- paste("Dim ", axes[1], " (", round(x$eigen[axes[1], 2], 2), " %)", sep = "")
  lab.y <- paste("Dim ", axes[2], " (", round(x$eigen[axes[2], 2], 2), " %)", sep = "")

  if (is.null(title)) titre <- "sensory mrCA graph" else titre <- title

  # ---- Calcul des ellipses de bootstrap ----
  # Le calcul est effectué uniquement si ellipse = TRUE
  if (ellipse) {
    nprods <- nlevels(x$bootstrap.replicate.coord$produit)
    prods  <- levels(x$bootstrap.replicate.coord$produit)

    ell <- data.frame(
      produit = as.factor(rep(prods, each = 100)),
      matrix(0, 100 * nprods, 2)
    )
    colnames(ell)[2:3] <- paste("Dim.", 1:2, sep = "")

    for (p in prods) {
      boot.rep.p  <- x$bootstrap.replicate.coord[x$bootstrap.replicate.coord$produit == p, -1]
      boot.rep.p  <- as.matrix(boot.rep.p)
      sigma       <- cov(boot.rep.p)
      mu          <- colMeans(boot.rep.p)
      calc.mal.sq <- function(vec) {
        mal.bary <- t(as.matrix(vec - mu)) %*% solve(sigma, tol = 1e-300) %*% (as.matrix(vec - mu))
        return(as.numeric(mal.bary))
      }
      mal.sq.cloud <- apply(boot.rep.p, 1, calc.mal.sq)
      dilat.stat   <- sqrt(quantile(mal.sq.cloud, 1 - alpha.ellipse, type = 2))
      ell.p        <- ellipse::ellipse(sigma[c(axes[1], axes[2]), c(axes[1], axes[2])],
                                       centre = mu[c(axes[1], axes[2])],
                                       t      = dilat.stat)
      ell[which(ell$produit == p), 2:3] <- ell.p
    }
  }

  # ---- Coordonnées descripteurs ----
  adjusted.col.coord <- x$desc.coord
  if (!is.null(select.desc)) {
    adjusted.col.coord <- adjusted.col.coord[select.desc, , drop = FALSE]
  }

  max.norm.prod <- max(abs(x$prod.coord[, axes]))
  max.norm.desc <- max(abs(adjusted.col.coord[, axes]))
  expand        <- max.norm.prod / max.norm.desc * expansion
  adjusted.col.coord <- adjusted.col.coord * expand

  # ---- Réversion des axes ----
  if (rev.x) {
    x$prod.coord[, axes[1]]       <- -x$prod.coord[, axes[1]]
    adjusted.col.coord[, axes[1]] <- -adjusted.col.coord[, axes[1]]
    if (ellipse) ell[, 2]          <- -ell[, 2]
  }
  if (rev.y) {
    x$prod.coord[, axes[2]]       <- -x$prod.coord[, axes[2]]
    adjusted.col.coord[, axes[2]] <- -adjusted.col.coord[, axes[2]]
    if (ellipse) ell[, 3]          <- -ell[, 3]
  }

  # ---- Limites du graphique ----
  xmin <- min(x$prod.coord[, axes[1]], adjusted.col.coord[, axes[1]], if (ellipse) ell[, 2])
  xmax <- max(x$prod.coord[, axes[1]], adjusted.col.coord[, axes[1]], if (ellipse) ell[, 2])
  ymin <- min(x$prod.coord[, axes[2]], adjusted.col.coord[, axes[2]], if (ellipse) ell[, 3])
  ymax <- max(x$prod.coord[, axes[2]], adjusted.col.coord[, axes[2]], if (ellipse) ell[, 3])

  if (is.null(xlim)) {
    xlim <- c(xmin, xmax)
    xlim <- (xlim - mean(xlim)) * 1.2 + mean(xlim)
  }
  if (is.null(ylim)) {
    ylim <- c(ymin, ymax)
    ylim <- (ylim - mean(ylim)) * 1.2 + mean(ylim)
  }

  # Rééquilibrage des axes si très asymétriques
  if (diff(xlim) / diff(ylim) > 3)   ylim <- (ylim - mean(ylim)) * diff(xlim) / diff(ylim) / 3 + mean(ylim)
  if (diff(xlim) / diff(ylim) < 1/2) xlim <- (xlim - mean(xlim)) * diff(ylim) / diff(xlim) / 2 + mean(xlim)

  # ---- Sélection individus ----
  test.invisible <- rep(NA, 2)
  if (!is.null(invisible)) {
    test.invisible[1] <- match("prod", invisible)
    test.invisible[2] <- match("desc", invisible)
  }

  # ---- Habillage produits ----
  color.prod <- rep(col.prod, nrow(x$prod.coord))
  if (habillage != "none") {
    if (!is.null(col.hab) && length(col.hab) == nrow(x$prod.coord)) {
      color.prod <- col.hab
    } else {
      color.prod <- palette[1:nrow(x$prod.coord)]
    }
  }

  # ========== GRAPHIQUE GGPLOT ==========
  if (new.plot & !nzchar(Sys.getenv("RSTUDIO_USER_IDENTITY"))) {
    dev.new(width = min(14, 8 * diff(xlim) / diff(ylim)), height = 8)
  }

  theme_mrca <- theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(
      hjust = 1,
      size  = if (is.null(argument[["cex.axis"]])) 10 else 10 * argument$cex.axis,
      face  = "bold"),
    plot.title = element_text(
      hjust = 0.5,
      size  = if (is.null(argument[["cex.main"]])) 11 else 11 * argument$cex.main,
      face  = "bold"),
    legend.position    = ifelse(legend$x %in% c("bottom", "up", "right", "left"), legend$x, "right"),
    legend.box.spacing = unit(0.1, "cm"),
    legend.margin      = margin()
  )

  gg_graph <- ggplot() +
    coord_fixed(ratio = 1) +
    theme_light() +
    theme_mrca +
    xlim(xlim[1], xlim[2]) +
    ylim(ylim[1], ylim[2]) +
    xlab(lab.x) + ylab(lab.y) +
    ggtitle(titre) +
    geom_hline(yintercept = 0,
               linetype  = ggoptions_default$line.lty,
               linewidth = ggoptions_default$line.lwd,
               color     = ggoptions_default$line.color) +
    geom_vline(xintercept = 0,
               linetype  = ggoptions_default$line.lty,
               linewidth = ggoptions_default$line.lwd,
               color     = ggoptions_default$line.color)

  # ---- Ellipses (conditionnelles) ----
  if (ellipse) {
    gg_graph <- gg_graph +
      geom_path(data    = as.data.frame(ell),
                mapping = aes(x = ell[, 2], y = ell[, 3], group = ell[, 1]),
                colour    = col.ellipse,
                linewidth = ggoptions_default$circle.lwd,
                linetype  = ggoptions_default$circle.lty)
  }

  # ---- Segments de non-différence ----
  diff.test  <- x$total.bootstrap.test.pvalues
  df.segment <- NULL
  for (i in 1:nrow(diff.test)) {
    for (j in i:ncol(diff.test)) {
      p.1 <- rownames(diff.test)[i]
      p.2 <- colnames(diff.test)[j]
      if (diff.test[p.1, p.2] > alpha.total.bootstrap.test & i != j) {
        ac.produit.coord <- as.data.frame(x$prod.coord[, axes])
        p.1.coord        <- ac.produit.coord[p.1, ]
        p.2.coord        <- ac.produit.coord[p.2, ]
        sous.df.segment  <- cbind(p.1.coord, p.2.coord)
        df.segment       <- rbind(df.segment, sous.df.segment)
      }
    }
  }
  if (!is.null(df.segment)) {
    colnames(df.segment) <- as.character(1:ncol(df.segment))
    gg_graph <- gg_graph +
      geom_segment(data    = as.data.frame(df.segment),
                   mapping = aes(x    = df.segment[, 1], y    = df.segment[, 2],
                                 xend = df.segment[, 3], yend = df.segment[, 4]),
                   colour    = col.segment,
                   linewidth = ggoptions_default$segment.lwd * 2.6)
  }

  # ---- Flèches descripteurs ----
  if (!is.null(select.desc) & is.na(test.invisible[2])) {
    df.fleche  <- as.matrix(adjusted.col.coord[, axes, drop = FALSE])
    col.fleche <- rep(col.desc, nrow(df.fleche))

    if (!is.null(x$proj.col.coord)) {
      proj.scaled <- as.matrix(x$proj.col.coord[, axes, drop = FALSE]) * expand
      df.fleche   <- rbind(df.fleche, proj.scaled)
      col.fleche  <- c(col.fleche, rep(col.proj, nrow(x$proj.col.coord)))
    }

    gg_graph <- gg_graph +
      geom_segment(data    = as.data.frame(df.fleche),
                   mapping = aes(x = 0, y = 0,
                                 xend = df.fleche[, 1],
                                 yend = df.fleche[, 2]),
                   arrow     = arrow(length = unit(size.head.arrow, "cm"), type = "closed"),
                   colour    = col.fleche,
                   linewidth = ggoptions_default$segment.lwd)

    # Labels descripteurs
    if (lab.desc) {
      lab.desc.df <- as.data.frame(df.fleche)
      lab.desc.df$label <- rownames(df.fleche)

      if (autoLab == "auto") autoLab <- (nrow(lab.desc.df) < 50)

      if (isTRUE(autoLab)) {
        gg_graph <- gg_graph +
          ggrepel::geom_text_repel(
            data     = lab.desc.df,
            mapping  = aes(x = df.fleche[, 1], y = df.fleche[, 2], label = label),
            colour   = col.fleche,
            size     = ggoptions_default$size * size.lab,
            fontface = "italic",
            min.segment.length = 1,
            max.overlaps = 50
          )
      } else {
        gg_graph <- gg_graph +
          geom_text(
            data     = lab.desc.df,
            mapping  = aes(x = df.fleche[, 1], y = df.fleche[, 2], label = label),
            colour   = col.fleche,
            size     = ggoptions_default$size * size.lab,
            fontface = "italic",
            hjust    = (-sign(df.fleche[, 1]) + 1) / 2,
            vjust    = -sign(df.fleche[, 2]) * 0.75 + 0.25
          )
      }
    }
  }

  # ---- Points produits ----
  if (is.na(test.invisible[1])) {
    df.point <- as.data.frame(x$prod.coord[, axes, drop = FALSE])

    if (habillage == "none") {
      gg_graph <- gg_graph +
        geom_point(data    = df.point,
                   mapping = aes(x = df.point[, 1], y = df.point[, 2]),
                   colour  = color.prod,
                   shape   = ggoptions_default$point.shape,
                   size    = ggoptions_default$size / 3 * size.points)
    } else {
      df.point$group <- rownames(df.point)
      gg_graph <- gg_graph +
        geom_point(data    = df.point,
                   mapping = aes(x = df.point[, 1], y = df.point[, 2], color = group),
                   shape   = ggoptions_default$point.shape,
                   size    = ggoptions_default$size / 3 * size.points) +
        scale_color_manual(values = palette[1:nrow(df.point)]) +
        labs(color = ifelse(!is.null(legend[["title"]]), legend[["title"]], "Produit"))
    }

    # Labels produits
    if (lab.prod) {
      nudge.prod <- as.matrix(x$prod.coord[, axes]) * 0.01

      if (autoLab == "auto") autoLab <- (nrow(df.point) < 50)

      if (isTRUE(autoLab)) {
        gg_graph <- gg_graph +
          ggrepel::geom_label_repel(
            data    = df.point,
            mapping = aes(x = df.point[, 1], y = df.point[, 2],
                          label = rownames(df.point)),
            colour        = if (habillage == "none") color.prod else palette[1:nrow(df.point)],
            size          = ggoptions_default$size * size.lab,
            label.size    = NA,
            label.padding = 0,
            nudge_x       = nudge.prod[, 1],
            nudge_y       = nudge.prod[, 2],
            min.segment.length = 1,
            max.overlaps  = 50,
            show.legend   = FALSE
          )
      } else {
        gg_graph <- gg_graph +
          geom_text(
            data    = df.point,
            mapping = aes(x = df.point[, 1], y = df.point[, 2],
                          label = rownames(df.point)),
            colour  = if (habillage == "none") color.prod else palette[1:nrow(df.point)],
            size    = ggoptions_default$size * size.lab,
            hjust   = (-sign(df.point[, 1]) + 1) / 2,
            vjust   = -sign(df.point[, 2]) * 0.75 + 0.25,
            show.legend = FALSE
          )
      }
    }
  }

  palette(old.palette)
  return(gg_graph)
}
