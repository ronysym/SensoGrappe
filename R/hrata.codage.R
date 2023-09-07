#' hrata.codage : function to prepare the codage file needed in hrata.agregation from the Hierachical structure CSV file
#'
#'
#' @param h.table dataframe (from a CSV file)  containing the Hierachical structure and structured with three columns and presenting the hierarchical organization of the dataset (wheel)
#' Family and Category are duplicated to express the hierarchical structure : 3 columns (Family,Category and Attribute)
#'
#' @seealso [hrata.signi()] [hrata.table()] [hrata.multidim()]
#'
#'
#' @examples
#' data(apple.attribute)
#' hrata.codage(apple.attribute)
#'
#'
#' @export
hrata.codage<-function(h.table)
  {
  data.table=h.table
  data.table=Var.Grappe(data.table,c(1:2), "factor",verbose = FALSE)
  nattribut<-nlevels(data.table[,1])+nlevels(data.table[,2])+length(data.table[,3])
  codage<-matrix(ncol=4,nrow=nattribut)
  colnames(codage)=c("attribut","categorie","famille","type")

  i<-1
  cc<-1
  ff<-1


  for (f in 1:nlevels(data.table[,1])){
    fam<-levels( factor(data.table[,1], as.character(unique(data.table[,1]))))[f]
    codage[i,1]<-as.character(fam)
    codage[i,2]<-NA
    codage[i,3]<-ff
    codage[i,4]<-"F"
    i<-i+1

    for (c in 1:length(which(duplicated(data.table[which(data.table[,1]==fam),2])==FALSE))){
      cat<-as.character(data.table[which(data.table[,1]==fam),2][which(duplicated(data.table[which(data.table[,1]==fam),2])==FALSE)][c])
      codage[i,1]<-as.character(cat)
      codage[i,2]<-cc
      codage[i,3]<-ff
      codage[i,4]<-"C"
      i<-i+1

      for (t in 1:length(data.table[which(data.table[,2]==cat),3])){
        codage[i,1]<-as.character(data.table[which(data.table[,2]==cat),3][t])
        codage[i,2]<-cc
        codage[i,3]<-ff
        codage[i,4]<-"A"
        i<-i+1
      }

      cc<-cc+1

    }
    ff<-ff+1
  }

  codage<-as.data.frame(codage)
  codage[,2]<-as.numeric(as.character(codage[,2]))
  codage[,3]<-as.numeric(as.character(codage[,3]))

  return(codage)

}

