#'Descri.Quanti : Boxplot  and graph for numeric variables
#'
#' @param x     					dataframe
#' @param column          column to be describe
#' @param group					  name of the column to use for grouping data
#' @param print.graph     logical. plot the graphs. TRUE by default
#' @param graph.grouped   logical. group th graph into one. TRUE by default
#' @param save.graph      logical. save the graphe. TRUE by default
#' @param date            logical. include date in the name of file : FALSE by default
#' @param extension       extension for the images(wmf, png, etc.)
#'
#' @import utils ggplot2 grid grDevices plyr gridExtra
#' @rawNamespace import(stats, except=step)
#' @examples
#'data(wine)
#'Descri.Quanti(wine, c(3:5),"ProductName")
#'
#' @export
Descri.Quanti <- function(x, column, group=NULL, print.graph=TRUE, graph.grouped=TRUE, save.graph=FALSE, date=FALSE, extension="wmf")
{
  if(!is.null(group) && class(group) != "character")
  {
    stop("L'argument group doit etre une chaine de caracteres (entre guillemets)")
  }
  #Enregistre la date du jour
  if(date == TRUE)
  {
    date.ori <- unlist(strsplit(date(), " ", fixed=TRUE))
    date.fin <- paste(date.ori[3], date.ori[2], date.ori[5], sep="_")
  }
  #Extrait le numero des colonnes si besoin
  if(class(column) == "character")
  {
    col.temp <- vector()
    for(c in 1:length(column))
    {
      verif <- match(x = column[c], table = colnames(x), nomatch = 0)
      if(verif == 0) { stop("Verifier les noms de colonnes", call. = FALSE) }
      col.temp[c] <- verif
    }
    column <- col.temp
  }

  for(c in column)
  {
    #Nom de la variable etudiee
    nom.var <- colnames(x)[c]
    #Dessine le boxplot
    bxplt <- ggplot(data=x, aes_string(x=factor(0), y=nom.var), color="black") +
      geom_boxplot() + theme_bw() + labs(x="", y=nom.var) +
      stat_summary(fun=mean, geom="point", shape=3, size=3) +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            panel.grid.major=element_blank(),
            plot.margin=unit(c(1, 0.5, 0, 0.5), "lines"))
    if(!is.null(group)) { bxplt <- bxplt +
      labs(title=group) +
      theme(legend.position="none") +
      aes_string(fill=group) +
      facet_grid(paste(". ~", group), scales="free_x") }
    #Dessine le bean/violin plot + Scatterplot
    violn <- ggplot(data=x, aes_string(x=factor(0), y=nom.var), color="black") +
      geom_violin(trim=TRUE) + labs(x="") +
      geom_jitter(position=position_jitter(width=0.1, height=0), alpha=0.3, shape=16) +
      theme_bw() + theme(axis.text.x=element_blank(),
                         axis.ticks.x=element_blank(),
                         panel.grid.major=element_blank(),
                         plot.margin=unit(c(1, 0.5, 0, 0.5), "lines"))
    if(!is.null(group)) { violn <- violn +
      labs(title=group) +
      theme(legend.position="none") +
      aes_string(fill=group) +
      facet_grid(paste(". ~ ", group), scales="free_x") }

    #Si demande, affiche / sauvegarde la figure finale
    if(graph.grouped == TRUE)
    {
      #Changement du nom des arguments !! (30-11-2015)
      plot.final <- arrangeGrob(bxplt, violn,  nrow=2, top=nom.var)
    }
    if(print.graph == TRUE && graph.grouped == TRUE)
    {
      grid.newpage()
      grid.draw(plot.final)
    } else
      {
      if(names(dev.cur()) == "null device" || names(dev.cur()) == "windows") { dev.new() }
      print(bxplt)
      if(names(dev.cur()) == "null device" || names(dev.cur()) == "windows") { dev.new() }
      print(violn)
    }
    #Enregistre les graphiques
    if(save.graph == TRUE)
    {
      name.final <- nom.var
      if(!is.null(group)) { name.final <- paste(name.final, "_", group, sep="") }
      if(date == TRUE) { name.final <- paste(name.final, "_", date.fin, sep="") }

      ggsave(filename=paste(name.final, extension, sep="."), path=getwd(), plot.final)
    }
  }
}
