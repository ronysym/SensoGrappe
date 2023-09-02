#' hrata.signi : Perform a regression analysis
#'
#' @description Perform a XXXXX analysis on the HRATA or HCATA data to identify discriminative attibutes
#'
#'
#' @param	res.agreg results from [hrata.agregation]
#' @param	seuil the significance threshold considered for the analyses of variance. Only attribute under this value will keep in the results (by default 0.05)
#'
#' @return Returns graphs of means by factors and variables with letters from posthoc test, standard deviations and pvalue from ANOVAs
#'
#'
#' @seealso [AOV.Grappe()]
#'
#' @import car
#'
#' @examples
#'data(apple)
#'data(apple.attribute)
#'res.agreg<-hrata.agregation(data=apple,h.table=apple.attribute,crit.agreg=max)
#'res.signi<-hrata.signi(res.agreg,seuil=0.05)
#'res.table<-hrata.table(res.agreg,type="dravnieks")
#'res.PCA<-hrata.multidim(res.table,method="PCA",scale.unit=FALSE,niv=3)
#'
#'
#' @export




hrata.signi<-function(res.agreg,seuil=NULL)
  {

  ajout<-2
  data<-res.agreg$hierarchical.data


 # pour gerer les variables en doubles dans le cas des roues à 2 niveaux
 # data<-data[,-c(which(duplicated(colnames(data))))]

  latt<-vector("list",ncol(data)-ajout)
  for (i in (ajout+1):ncol(data)){
    suj<-data[which(data[,i]>0),1]  # s?lectionne les sujets qui ont ?valu? l'attribut i pour n'importe quel produit
    if (length(which(duplicated(suj)))==0){ # v?rifie s'il y a des duplicats dans les sujets s?lectionn?s
      suj<-suj
    }else{
      suj<-suj[-which(duplicated(suj))] # si duplicats, on les enl?ve --> on obtient la liste des sujets qui ont s?lectionn? l'atttribut i pour au moins un des produits
    }# s?lection des sujets qui ont au moins mis 1 note d'intensit? ? l'attribut pour un des produits
    vec<-NULL
    for (s in 1:length(suj)){
      vec<-c(vec,which(data[,1]==suj[s]))
    }
    latt[[i-ajout]]<-data[vec,c(1,2,i)]
  }

  reg2<-vector(length=length(latt))
  names(reg2)<-colnames(data)[(ajout+1):ncol(data)]
  for (i in 1:length(latt)){
    dd<-latt[[i]]
    if (nrow(dd)>24){

      dd[,3]<-replace(dd[,3],dd[,3]>0,1)

      response<-dd[,3]
      produit<-dd[,2]
      sujet<-dd[,1]

      res.reg2<-suppressWarnings({glm(response~sujet+produit,family=binomial)})
      res2<-Anova(res.reg2,type=3)
      reg2[i]<-res2$`Pr(>Chisq)`[2]

    }  else {
      reg2[i]<-1
    }
  }
  if (is.null(seuil))  {
    return(reg2)
  }else{

    out<-reg2[which(reg2<seuil)]
    return(out)
  }
}
