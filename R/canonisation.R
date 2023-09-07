#' canonisation : transpose a multivariable table in one column
#'
#' @description transpose a multivariable table in one column
#'
#' @param	data.canon  a dataframe to be change from a multiple column to a two column wit a replication of the first ones
#' @param	NbLeft  a value indicating the number of column on the left of the table that do no be concatened
#'
#'
#' @return a dataframe with the NbLeft columns, a column with the canonized variables in "Attribute" column and the value in "Score" column
#'
#' @examples
#' data(wine)
#' Var.Grappe(wine)
#' winef=Var.Grappe(wine,column = c(1:2), type="factor")
#' canonisation(winef,2)
#'
#'
#' @export
canonisation<-function(data.canon,NbLeft){

tutu<-NULL
for (i in 1:(length(colnames(data.canon))-NbLeft)){ tutu=rbind(tutu, cbind(data.canon[,c(1:NbLeft)],colnames(data.canon[i+NbLeft]),data.canon[,i+NbLeft] ))}
colnames(tutu)[c(NbLeft+1,NbLeft+2)]<-c("Attribute","Score")
tutu<-Var.Grappe(tutu,c(1:NbLeft+1),"factor", verbose=FALSE)
out<-tutu

return(out)}

