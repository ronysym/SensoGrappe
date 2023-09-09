#' hrata.multidim : Performs a multidimensionnal analysis PCA for HRATA and HCATA data
#'
#' @description Performs a multidimensionnal analysis PCA for HRATA data and HCATA data
#'
#'
#'@param res.table  a dataframe provided by [hrata.table]
#'@param method  "PCA" for HRATA or "CA" for HCATA and HRATA
#'@param scale.unit  a boolean, if TRUE (value set by default) then data are scaled to unit variance
#'@param niv a value to be fixe at 2 if the number of hierarichie is 2
#'@param select a value indicating which level are used as active variables  : "cat" for category, "fam" for familly, "att" for terms (by defaul NULL)
#'
#' @return Returns a list similar to [PCA]
#'
#'
#' @import FactoMineR
#'
#' @seealso [hrata.signi()] [hrata.agregation()] [hrata.table()]
#'
#'
#' @examples
#'data(rose)
#'data(rose.attribute)
#'res.agreg<-hrata.agregation(data=rose,h.table=rose.attribute,crit.agreg=max)
#'res.signi<-hrata.signi(res.agreg,seuil=0.05)
#'res.table<-hrata.table(res.agreg,type="dravnieks")
#'res.PCA<-hrata.multidim(res.table,method="PCA",scale.unit=FALSE,niv=3)
#'
#'
#' @export

hrata.multidim<-function(res.table,method="PCA",scale.unit=TRUE,niv=3,select=NULL)
{

  data.rata<-res.table$data.table
  att<-unlist(res.table$lcat.name)
  cat<-names(res.table$lcat.name)
  fam<-names(res.table$lfam.name)


  if (niv==2)
    {

    print("Demander a Ronan Symoneaux de faire cela...")

  } else
    {

    if (is.null(select))
      {
      sup<-NULL
    }else if (select=="cat"){
      sup<-which(colnames(data.rata)%in%c(att,fam))
    }else if (select=="fam"){
      sup<-which(colnames(data.rata)%in%c(att,cat))
    }else if (select=="att"){
      sup<-which(colnames(data.rata)%in%c(fam,cat))
    }

    if (method=="PCA"){
      res.multidim<-PCA(data.rata,scale.unit=scale.unit,graph=TRUE,quanti.sup=sup)
      return(res.multidim)

    } else if (method=="CA")
      {


      hcata.conting<-(t(as.data.frame(data.rata[data.rata$Amplitude >= 5,1:(length(colnames(data.rata))-4)])))
      res.multidim<-CA(hcata.conting,quanti.sup=sup,graph=TRUE)


      dev.new()
      rx=range(res.multidim$row$coord[,1],res.multidim$col$coord[,1])
      ry =range(res.multidim$row$coord[,2],res.multidim$col$coord[,2])
      plot (rx,ry,col = NA,xlab=paste("Dim 1 (", signif(res.multidim$eig[1,2],4), "%)") ,ylab=paste("Dim 2 (", signif(res.multidim$eig[2,2],4), "%)"))
      points(res.multidim$row$coord[,1],res.multidim$row$coord[,2],col = "red",pch =21,bg="cornsilk",cex = (res.multidim$row$contrib[,1]+res.multidim$row$contrib[,2])*0.3 )
      text(res.multidim$row$coord[,1],res.multidim$row$coord[,2], row.names(res.multidim$row$coord), pos = 4, col = "red", cex = .7)
      points(res.multidim$col$coord[,1],res.multidim$col$coord[,2], col = "blue", pch =24,bg="lightcyan",cex = (res.multidim$col$contrib[,1]+res.multidim$col$contrib[,2])*0.05)
      text(res.multidim$col$coord[,1],res.multidim$col$coord[,2], row.names(res.multidim$col$coord), pos = 1, col = "blue", cex = .7)

      # For the creation of the CA graph
      dev.new()
      rx=range(res.multidim$row$coord[,3],res.multidim$col$coord[,3])
      ry =range(res.multidim$row$coord[,4],res.multidim$col$coord[,4])
      plot (rx,ry,col = NA,xlab=paste("Dim 3 (", signif(res.multidim$eig[3,2],4), "%)") ,ylab=paste("Dim 4 (", signif(res.multidim$eig[4,2],4), "%)"))
      points(res.multidim$row$coord[,3],res.multidim$row$coord[,4],col = "red",pch =21,bg="cornsilk",cex = (res.multidim$row$contrib[,3]+res.multidim$row$contrib[,4])*0.3 )
      text(res.multidim$row$coord[,3],res.multidim$row$coord[,4], row.names(res.multidim$row$coord), pos = 4, col = "red", cex = .7)
      points(res.multidim$col$coord[,3],res.multidim$col$coord[,4], col = "blue",  pch =24,bg="lightcyan",cex = (res.multidim$col$contrib[,3]+res.multidim$col$contrib[,4])*0.05)
      text(res.multidim$col$coord[,3],res.multidim$col$coord[,4], row.names(res.multidim$col$coord), pos = 1, col = "blue", cex = .7)

      return(res.multidim)



    }

  }

}
