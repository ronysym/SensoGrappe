#' hrata.table : Compute the average table for HRATA or HCATA multidimensionnal result
#'
#' @description Compute the dravnieks or mean table from HRATA results or the contingency table for HCATA as defined in Koenig et al (2023)
#'
#' @param	res.agreg results from [hrata.agregation]
#' @param type  aggregation criterion of the average table ("mean" or "dravnieks" or "contingency") by default "dravnieks"
#'
#'
#' @import FactoMineR stringr
#' @rawNamespace import(factoextra, except=hcut)
#' @seealso [hrata.signi()] [hrata.agregation()] [hrata.multidim()]
#'
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


hrata.table<-function(res.agreg,type="dravnieks")
    {

    if (length(res.agreg)>1)
      {

      ajout<-2

      lfam.name<-res.agreg$lfam.name
      lcat.name<-res.agreg$lcat.name
      natt<-length(unlist(lcat.name))
      ncat<-length(lcat.name)
      nfam<-length(lfam.name)
      res.agreg$hierarchical.data$Produit=as.factor(res.agreg$hierarchical.data$Produit)
      npdt<-nlevels(res.agreg$hierarchical.data$Produit)

      data<-res.agreg$hierarchical.data

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



      if (type=="mean"){ # a modifier sur la base de latt

        data.table<-matrix(nrow=6,ncol=ncol(data)-ajout)
        for (i in 1:length(latt)){
          for (k in 1:nlevels(data$Produit)){
            data.table[k,i]<-mean(latt[[i]][which(latt[[i]][,ajout]==levels(data$Produit)[k]),][,ajout+1])
          }
        }
        data.table[is.na(data.table)]<-0

        colnames(data.table)<-colnames(data)[3:ncol(data)]
        rownames(data.table)<-levels(data[,ajout])
        data.table=as.data.frame(data.table)
        res.table<-list(data.table,lfam.name,lcat.name)
        names(res.table)<-c("data.table","lfam.name","lcat.name")
        return(res.table)

      }else{

        if (type=="dravnieks"){

          # calcul du score de dravnieks
          data.table<-matrix(nrow=npdt,ncol=ncol(data)-ajout)
          echelle<-max(data[(ajout+1):ncol(data)])

          for (i in 1:length(latt)){
            for (k in 1:nlevels(data$Produit)){

              pik<-length(which(latt[[i]][latt[[i]][,ajout]==levels(data$Produit)[k],(ajout+1)]>0)) # nombre de sujets ayant donn? une note positive ? l'attribut i pour le produit k
              pk<-length(latt[[i]][latt[[i]][,ajout]==levels(data$Produit)[k],3]) # nombre de sujets ayant ?valu? l'attribut i pour au moins 1 produit
              xi<-sum(latt[[i]][latt[[i]][,ajout]==levels(data$Produit)[k],3]) # somme des notes de l'attribut i pour le produit k

              x<-echelle*pk # somme des notes max pour les sujets qui ont ?valu? l'attribut i pour un moins un  des produits

              data.table[k,i]<-sqrt((pik/pk*100)*(xi/x*100))
            }
          }
          data.table[is.na(data.table)]<-0
          colnames(data.table)<-colnames(data)[(ajout+1):ncol(data)]
          rownames(data.table)<-levels(data[,ajout])
          data.table=as.data.frame(data.table)
          res.table<-list(data.table,lfam.name,lcat.name)
          names(res.table)<-c("data.table","lfam.name","lcat.name")
          return(res.table)

        } else{
          print("Type not valid")
        }
      }
    }else
      {
      data<-res.agreg$hierarchical.data

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


      if (type=="mean"){ # a modifier sur la base de latt

        data.table<-matrix(nrow=6,ncol=ncol(data)-ajout)
        for (i in 1:length(latt)){
          for (k in 1:nlevels(data$Produit)){
            data.table[k,i]<-mean(latt[[i]][which(latt[[i]][,ajout]==levels(data$Produit)[k]),][,ajout+1])
          }
        }
        data.table[is.na(data.table)]<-0

        colnames(data.table)<-colnames(data)[3:ncol(data)]
        rownames(data.table)<-levels(data[,ajout])
        data.table=as.data.frame(data.table)
        return(data.table)

      }else{

        if (type=="dravnieks")
          {

          # calcul du score de dravnieks
          data.table<-matrix(nrow=npdt,ncol=ncol(data)-ajout)
          echelle<-max(data[(ajout+1):ncol(data)])

          for (i in 1:length(latt)){
            for (k in 1:nlevels(data$Produit)){

              pik<-length(which(latt[[i]][latt[[i]][,ajout]==levels(data$Produit)[k],(ajout+1)]>0)) # nombre de sujets ayant donn? une note positive ? l'attribut i pour le produit k
              pk<-length(latt[[i]][latt[[i]][,ajout]==levels(data$Produit)[k],3]) # nombre de sujets ayant ?valu? l'attribut i pour au moins 1 produit
              xi<-sum(latt[[i]][latt[[i]][,ajout]==levels(data$Produit)[k],3]) # somme des notes de l'attribut i pour le produit k

              x<-echelle*pk # somme des notes max pour les sujets qui ont ?valu? l'attribut i pour un moins un  des produits

              data.table[k,i]<-sqrt((pik/pk*100)*(xi/x*100))
            }
          }
          data.table[is.na(data.table)]<-0
          colnames(data.table)<-colnames(data)[(ajout+1):ncol(data)]
          rownames(data.table)<-levels(data[,ajout])
          data.table=as.data.frame(data.table)

          return(data.table)

        } else{
          print("Type not valid")
        }
      }
    }
  }
