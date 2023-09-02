#' hcata.table : prepare the hcata table for analysis
#'
#' @description create the dataframe use for HCATA analysis
#'
#' @param	res.agreg  a dataframe xxxxxxxx
#'
#' @import graphics
#' @return returns graphs of means by factors and variables with letters from posthoc test, standard deviations and pvalue from ANOVAs
#'
#'
#' @seealso xxxxxx
#'
#' @examples
#'data(apple)
#'data(apple.attribute)
#'res.agreg<-hrata.agregation(data=apple,h.table=apple.attribute,crit.agreg=max)
#'res.signi<-hrata.signi(res.agreg,seuil=0.05)
#'res.table<-hcata.table(res.agreg)
#'res.PCA<-hrata.multidim(res.table,method="CA",niv=3)
#'
#' @export
#'
hcata.table<-function(res.agreg)
  {
  lfam.name<-res.agreg$lfam.name
  lcat.name<-res.agreg$lcat.name
  data.hcata<-canonisation(res.agreg$hierarchical.data,2)
  data.hcata$Score[data.hcata$Score>1]=1
  data.hcata$Score[data.hcata$Score==0]=NA

  #creation du tableau de contingence
  mytable <- xtabs(Score~ Attribute+Produit, data=data.hcata)
  attributes(mytable)$class <- "table"
  mytable=addmargins(mytable, 2,FUN = list(list(Sum=sum,Max=max,Min=min)),quiet=TRUE)
  mytable=cbind(mytable,mytable[,"Max"]-mytable[,"Min"])
  colnames(mytable)[length(colnames(mytable))]= "Amplitude"
  mytable=as.data.frame(mytable)

  #Graph de la selection des attributs
  mytable=mytable[order(mytable[,"Sum"]), ]
  dev.new()
  barplot(mytable$Sum,axes=F,horiz=T,plot=T,col="#FFCC66", names=rownames(mytable),las=1, cex.names=0.5,cex.axis=0.5)
  axis(3,pretty(mytable$Attribute,2),col="#F5821F", cex.axis=0.5)

  mytable=mytable[order(mytable[,"Amplitude"]), ]
  dev.new()
  barplot(mytable$Amplitude,axes=F,horiz=T,plot=T,col="#FFCC66", names=rownames(mytable),las=1, cex.names=0.5,cex.axis=0.5)
  axis(3,pretty(mytable$Attribute,2),col="#F5821F", cex.axis=0.5)



  data.table =as.data.frame(mytable)
  res.table<-list(data.table,lfam.name,lcat.name)
  names(res.table)<-c("data.table","lfam.name","lcat.name")
  return(res.table)



}


