#' hrata.agregation : prepare the hrata table for analysis
#'
#' @description create the dataframe use for HRATA analysis
#'
#' @param	data  a dataframe containing rawdata HRATA or HCATA sensory task wiht Juge in first column and Product in the second column
#' @param	h.table  a dataframe with the hierarchical structure : three columns Familly, Category, Terms
#' @param	crit.agreg agregation criterion used during the data allocation : max, min, or mean
#'
#'
#'
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
#'res.table<-hrata.table(res.agreg,type="dravnieks")
#'res.PCA<-hrata.multidim(res.table,method="PCA",scale.unit=FALSE,niv=3)
#'
#'
#' @export
hrata.agregation<-function(data,h.table,crit.agreg=max)
{

  data=Var.Grappe(data, 3:length(data), "numeric",verbose=FALSE)
  colnames(data)[1:2]<-c("Juge","Produit")
  data=Var.Grappe(data,column =c("Juge","Produit"),type = "factor" ,verbose=FALSE)

  codage<-hrata.codage(h.table)

  natt<-ncol(data)-2 #nombre d'attributs (famille+categorie+termes)
  npdt<-nlevels(data$Produit) #nombre de produits
  nsujet<-nlevels(data$Juge) # nombre de sujets
  nniv<-3 #nombre de niveaux hierarchiques
  nfam<-max(codage$famille) #nombre de famille
  ncat<-max(codage$categorie,na.rm=TRUE) #nombre de categories


  ################################################################
  ## listes pour recuperer les colonnes des categories et familles

  lfam<-vector("list", nfam) # liste avec les numeros de colonnes des cat?gories dans les familles
  lfam.name<-vector("list", nfam) # liste avec les noms des attributs des cat?gories dans les familles
  lcatinfam<-vector("list", nfam) # liste avec les numeros des cat?gories et des familles
  lcat<-vector("list",ncat) # liste avec les numeros de colonnes des termes dans les cat?gories
  lcat.name<-vector("list",ncat) # liste avec le noms des attributs des termes dans les cat?gories
  cat<-0
  for (fam in 1: nfam) {
    elim<-c()
    loc<-which(codage$famille==fam)
    elim<-which(is.na(codage[loc,2]))
    if (length(elim)>0) loc<-loc[-elim]
    numcat<-c(unique(codage[loc,2]))
    nc<-length(numcat)
    for (c in 1:nc) {
      locc<-which(codage$categorie==numcat[c])
      lfam[[fam]]<-c(lfam[[fam]],locc[1])
      lfam.name[[fam]]<-as.character(codage$attribut[lfam[[fam]]])
      lcatinfam[[fam]][[c]]<-locc[-1]
      cat<-cat+1
      lcat[[cat]]<-locc[-1]
      lcat.name[[cat]]<-as.character(codage$attribut[lcat[[cat]]])
    }
  }

  names(lfam.name)<-as.character(codage$attribut[codage$type=="F"])

  names(lcat.name)<-as.character(codage$attribut[codage$type=="C"])

  if (all(is.na(names(lfam.name)))){names(lfam.name)<-names(lcat.name)} # si pas de famille alors on double les cat?gories

  ######################
  ## Construction des tableaux des attributs, categories et familles

  # on passe tous les NA en 0
  data[is.na(data)]<-0
  hierarchical.data<-data

  # matrice avec les donn?es termes
  data.att<-data[,unlist(lcatinfam)+2]
  data.att<-cbind(data[,1:2],data.att)
  hierarchical.term<-data.att

  # matrice avec les donn?es cat?gories
  data.cat<-matrix(nrow=nrow(data),ncol=ncat)
  i<-1
  for (f in 1:nfam){
    nc<-length(lcatinfam[[f]])
    for (c in 1:nc) {
      datc<-cbind(data[,lfam[[f]][c]+2],data[,lcatinfam[[f]][[c]]+2])
      data.cat[,i]<-apply(datc,1,crit.agreg)
      i<-i+1
    }
  }
  colnames(data.cat)<-as.character(codage$attribut[codage$type=="C"])
  data.cat<-cbind(data[,1:2],data.cat)
  hierarchical.cat<-data.cat

  # matrice avec les donn?es familles


  if (length(names(lfam.name))==length(names(lcat.name))) { # si pas de famille alors on bouble les cat?gories pour les familles
    hierarchical.fam<-hierarchical.cat
  } else
    {


    data.fam<-matrix(nrow=nrow(data),ncol=nfam)
    for (f in 1:nfam){
      datf=cbind(data[,which(codage$type=="F")[f]+2],data[,lfam[[f]]+2],data[,unlist(lcatinfam[[f]])+2])
      data.fam[,f]<-apply(datf,1,crit.agreg)
    }
    colnames(data.fam)<-codage$attribut[codage$type=="F"]
    data.fam<-cbind(data[,1:2],data.fam)
    hierarchical.fam<-data.fam

  }

  hierarchical.data<-cbind(hierarchical.fam,hierarchical.cat[,3:ncol(hierarchical.cat)],hierarchical.term[,3:ncol(hierarchical.term)])
  res.agreg<-list(hierarchical.data,lfam.name,lcat.name)
  names(res.agreg)<-c("hierarchical.data","lfam.name","lcat.name")

  return(res.agreg)
}
