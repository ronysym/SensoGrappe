#' hrata.signi : Perform a regression analysis
#'
#' @description Perform a logistic regression was  performed for each attribute, with subject and product as independent factors (Dobson and Barnett, 2008) for HRATA or HCATA data to identify discriminative attibutes
#'              as recommended  in Koenig et al (2023)
#'
#' @param	res.agreg results from [hrata.agregation]
#' @param	select.judge logical. (By default = TRUE) if TRUE subjects who did not consider an attribute were excluded from the analysis of that attribute so that the number of subjects for each attribute varied. If FALSE : all subjects are used fo all attributes
#' @param	seuil numeric. the significance threshold considered for the analyses of variance. Only attribute under this value will keep in the results (by default 0.05)
#'
#' @return Returns a dataframe with the pvalue for each attributes
#'
#'
#' @seealso [AOV.Grappe()]
#'
#' @import car
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




hrata.signi<-function(res.agreg, select.judge=FALSE, seuil=NULL)
  {

  data<-res.agreg$hierarchical.data


 # pour gerer les variables en doubles dans le cas des roues à 2 niveaux
 # data<-data[,-c(which(duplicated(colnames(data))))]



  latt<-vector("list",ncol(data)-2)

  for (i in 3:ncol(data)){


    if (select.judge==TRUE){ # verifie s'il y a des duplicats dans les sujets selectionnes
      suj<-data[which(data[,i]>0),1]   # selectionne les sujets qui ont evalue l'attribut i pour n'importe quel produit
      }else{
      suj<-data[which(data[,i]>=0),1] # si duplicats, on les enleve --> on obtient la liste des sujets qui ont selectionne l'atttribut i pour au moins un des produits
    }

     if (length(which(duplicated(suj)))==0){ # verifie s'il y a des duplicats dans les sujets selectionnes
      suj<-suj
    }else{
      suj<-suj[-which(duplicated(suj))] # si duplicats, on les enleve --> on obtient la liste des sujets qui ont selectionne l'atttribut i pour au moins un des produits
    }
    # selection des sujets qui ont au moins mis 1 note d'intensit? ? l'attribut pour un des produits
    vec<-NULL
    for (s in 1:length(suj)){
      vec<-c(vec,which(data[,1]==suj[s]))
    }
    latt[[i-2]]<-data[vec,c(1,2,i)]
  }


  reg2 <- as.data.frame(matrix(data=NA, nrow=length(latt), ncol=2))
  rownames(reg2)<-colnames(data)[3:ncol(data)]
  names(reg2)<-c("pvalue","NbrJuge")


  for (i in 1:length(latt)){
    dd<-latt[[i]]
    dd[,3]<-replace(dd[,3],dd[,3]>0,1)
    response<-dd[,3]
    produit<-dd[,2]
    sujet<-dd[,1]

    if (nlevels(droplevels(sujet))>2){
      res.reg2<-suppressWarnings({glm(response~sujet+produit,family=binomial)})
      res2<-suppressWarnings({Anova(res.reg2,type=3)})
      reg2[i,1]<-res2$`Pr(>Chisq)`[2]
      reg2[i,2]<-nlevels(droplevels(sujet))

    }else {
      reg2[i,1]<-1
    }
  }

   if (is.null(seuil))  {
    return(reg2)
  }else{

    out<-reg2[which(reg2$pvalue<seuil),]
    return(out)
  }
}
