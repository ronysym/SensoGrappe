#' Clustering et agrégation de descripteurs binaires
#'
#' @description
#' Regroupe des descripteurs binaires (présence/absence) en clusters de
#' façon à maximiser l'association entre les clusters résultants et une
#' variable catégorielle (produit, groupe, etc.). L'algorithme est fondé
#' sur une statistique de chi-deux multi-réponses (\emph{mr.chisqNS}) :
#' deux descripteurs sont fusionnés si leur fusion n'entraîne pas de perte
#' significative d'information vis-à-vis de la variable catégorielle.
#' Une étape optionnelle de transfert permet de réaffecter individuellement
#' des descripteurs entre clusters pour améliorer la solution.
#'
#' @param data Un `data.frame` dont :
#'   \itemize{
#'     \item la **première colonne** est un `factor` représentant la
#'       variable catégorielle (ex. : produit, modalité) ;
#'     \item les **colonnes suivantes** sont des entiers ou numériques
#'       binaires (0/1) représentant la présence/absence de chaque
#'       descripteur.
#'   }
#' @param transfer Logique. Si `TRUE` (défaut), une étape de transfert
#'   est réalisée après la phase de fusion : chaque descripteur est
#'   testé individuellement pour un éventuel changement de cluster si
#'   cela améliore le critère.
#'
#' @return Une liste contenant :
#'   \describe{
#'     \item{`clust`}{Vecteur entier nommé indiquant le numéro de cluster
#'       affecté à chaque descripteur (les noms correspondent aux noms
#'       des colonnes de `data[-1]`). Deux descripteurs portant le même
#'       numéro appartiennent au même cluster.}
#'     \item{`criterion`}{Liste retournée par la fonction interne
#'       \code{mr.chisqNS}, contenant :
#'       \itemize{
#'         \item `statistic` : valeur de la statistique chi-deux
#'           multi-réponses finale ;
#'         \item `p.value` : p-valeur associée.
#'       }}
#'   }
#'
#' @details
#' ## Algorithme de fusion (phase principale)
#' L'algorithme procède de façon gloutonne (\emph{greedy}) :
#' \enumerate{
#'   \item La statistique globale \emph{mr.chisqNS} est calculée sur la
#'     configuration courante.
#'   \item Pour chaque paire de clusters candidats (appartenant au même
#'     groupe défini par `common`), on évalue la variation de p-valeur
#'     (\eqn{\Delta p}) induite par leur fusion.
#'   \item La paire induisant la plus faible variation (la moins
#'     défavorable, i.e. \eqn{\Delta p \leq 0}) est fusionnée.
#'   \item L'algorithme s'arrête dès qu'aucune fusion n'améliore ou ne
#'     préserve le critère.
#' }
#'
#' ## Étape de transfert (optionnelle)
#' Après la phase de fusion, si `transfer = TRUE`, chaque descripteur
#' est examiné individuellement : il est déplacé vers un autre cluster
#' si ce transfert augmente la somme des chi-deux des deux clusters
#' concernés. Cette étape est répétée jusqu'à convergence.
#'
#' ## Statistique mr.chisqNS
#' La statistique multi-réponses est définie comme la somme des
#' statistiques de chi-deux calculées séparément pour chaque descripteur
#' (ou cluster agrégé) vis-à-vis de la variable catégorielle. La
#' p-valeur globale est obtenue par comparaison à une loi \eqn{\chi^2}
#' avec \eqn{(K-1) \times J} degrés de liberté, où \eqn{K} est le nombre
#' de niveaux de la variable catégorielle et \eqn{J} le nombre de
#' descripteurs (ou clusters).
#'
#' @section Contraintes sur les données :
#' \itemize{
#'   \item La première colonne doit être de classe `factor`.
#'   \item Les colonnes descripteurs doivent être de classe `numeric` ou
#'     `integer` et ne contenir que des valeurs 0 et 1.
#'   \item `data` doit être un `data.frame`.
#' }
#'
#' @seealso \code{\link{data.preprocess.fc}}, \code{\link{get.binary}}
#'
#' @examples
#' \dontrun{
#' # Création d'un jeu de données exemple
#' set.seed(42)
#' n <- 60
#' produit <- factor(rep(c("A", "B", "C"), each = n / 3))
#' desc1   <- rbinom(n, 1, ifelse(produit == "A", 0.8, 0.2))
#' desc2   <- rbinom(n, 1, ifelse(produit == "A", 0.7, 0.3))
#' desc3   <- rbinom(n, 1, ifelse(produit == "B", 0.9, 0.1))
#' desc4   <- rbinom(n, 1, 0.5)  # descripteur non discriminant
#'
#' dta <- data.frame(produit, desc1, desc2, desc3, desc4)
#'
#' # Clustering des descripteurs
#' res <- mr.clust(dta, transfer = TRUE)
#'
#' # Affichage des clusters
#' res$clust
#' #> desc1 desc2 desc3 desc4
#' #>     1     1     2     3   # desc1 et desc2 regroupés
#'
#' # Critère final
#' res$criterion$statistic
#' res$criterion$p.value
#' }
#'
#' @export



mr.clust=function(data,transfer=TRUE){
  common=rep("all",ncol(data)-1) # forcing tout le monde car filtre avant
  names(common)=as.character(1:length(common))
  classe = class(data)[1]
  if (!classe %in% c("data.frame")) {
    stop("data must a data.frame")
  }
  classe = class(data[,1])
  if (!classe %in% c("factor")) {
    stop("First column of data must be a factor")
  }
  for (j in 2:ncol(data)) {
    classe = class(data[, j])[1]
    if (!classe %in% c("numeric", "integer")) {
      stop("contingency data must be integer or numeric")
    }
  }
  vec.cat=data[,1]
  mat.bin=as.matrix(data[,-1])
  check.bin = unique(as.vector(mat.bin))
  if (length(check.bin) > 2) {
    stop("contingency data are not composed of only ones and zeros")
  } else {
    check.un = sum(check.bin == c(0, 1))
    check.deux = sum(check.bin == c(1, 0))
    if (check.un != 2 & check.deux != 2) {
      stop("contingency data are not composed of only ones and zeros")
    }
  }
  if (is.character(common)){
    if (length(common)!=(ncol(data)-1)){
      stop("length(common) must equal (ncol(data)-1)")
    }
  }else{
    stop("class(common) must be character")
  }
  mr.chisqNS=function(vec.cat,mat.bin){
    chisqbyrep=function(rep){
      rep=as.factor(rep)
      reptab=table(vec.cat,rep)
      return(chisq.test(reptab)$statistic)
    }
    vec.chisq=suppressWarnings(apply(mat.bin, 2, chisqbyrep))
    statistic=sum(vec.chisq)
    p.value=pchisq(statistic,(nlevels(vec.cat)-1)*length(vec.chisq),lower.tail = F)
    return(list(statistic=statistic,p.value=p.value))
  }
  evaluate.chisqNS=function(cat,rep){
    if (!is.factor(cat)){
      stop("cat must be a factor")
    }
    rep=as.factor(rep)
    reptab=table(cat,rep)
    chisqNS=suppressWarnings(chisq.test(reptab)$statistic)
    return(chisqNS)
  }
  f.vec2=function(vec2){
    candidat.rep=as.numeric(vec1==1|vec2==1)
    candidat.chisqNS=curchisqNS-evaluate.chisqNS(vec.cat,vec1)-evaluate.chisqNS(vec.cat,vec2)+evaluate.chisqNS(vec.cat,candidat.rep)
    candidat.p=pchisq(candidat.chisqNS,(ncol(curbin)-1)*(nlevels(vec.cat)-1),lower.tail = F)
    candidat.deltap=candidat.p-curp
    return(candidat.deltap)
  }
  deltap=-1
  curclust=1:ncol(mat.bin)
  ntour=0
  ntourmax=sum(table(common)[table(common)>1])
  curbin=mat.bin
  colnames(curbin)=paste("cl",curclust,sep=".")
  while (deltap<=0){
    bestdeltap=1e-16
    curp=mr.chisqNS(vec.cat,curbin)$p.value
    curchisqNS=mr.chisqNS(vec.cat,curbin)$statistic
    ucurclust=unique(curclust)
    for (id1 in 1:(length(ucurclust)-1)){
      candidat1=ucurclust[id1]
      common1=common[candidat1]
      vec1=curbin[,id1]
      groupe.candidat=ucurclust[-c(1:id1)]
      groupe.candidat=groupe.candidat[common[groupe.candidat]==common1]
      if (length(groupe.candidat)>0){
        groupe.candidat=paste("cl",groupe.candidat,sep=".")
        candidat.deltap=apply(curbin[,groupe.candidat,drop=FALSE], 2, f.vec2)
        if (any(candidat.deltap<bestdeltap)){
          bestdeltap=min(candidat.deltap)
          bestcandidat1=candidat1
          bestcandidat2=as.integer(substring(names(candidat.deltap)[which.min(candidat.deltap)],4))
        }
      }
    }
    deltap=bestdeltap
    if (deltap<=0){
      modifclust=curclust
      modifclust[modifclust==bestcandidat2]=bestcandidat1
      curclust=modifclust
      vec.cl=paste("cl",c(bestcandidat1,bestcandidat2),sep=".")
      curbin[,vec.cl[1]]=apply(curbin[,vec.cl],1,max)
      curbin=curbin[,-which(colnames(curbin)==vec.cl[2]),drop=FALSE]
      ntour=ntour+1
    }
  }
  
  #### transfer
  if (transfer){
    go=TRUE
    while(go){
      go=FALSE
      for (d in ncol(mat.bin):1){
        clust.d=paste("cl",curclust[d],sep=".")
        if (sum(curclust==curclust[d])>1){
          lien.clust.common=common[unique(curclust)]
          names(lien.clust.common)=paste("cl",names(lien.clust.common),sep=".")
          common.d=lien.clust.common[clust.d]
          clust.candidat=lien.clust.common[lien.clust.common==common.d]
          clust.candidat=clust.candidat[names(clust.candidat)!=clust.d]
          if (length(clust.candidat)>0){
            select.parti=curclust==as.numeric(substring(clust.d,4))
            select.parti[d]=FALSE
            desc.parti=apply(mat.bin[,select.parti,drop=FALSE],1,max)
            bestdeltachi=0
            for (clust.can in names(clust.candidat)){
              curchi=evaluate.chisqNS(vec.cat,curbin[,clust.d])+evaluate.chisqNS(vec.cat,curbin[,clust.can])
              desc.arrive=as.numeric(mat.bin[,d]==1|curbin[,clust.can]==1)
              canchi=evaluate.chisqNS(vec.cat,desc.parti)+evaluate.chisqNS(vec.cat,desc.arrive)
              if (canchi-curchi>bestdeltachi){
                bestdeltachi=canchi-curchi
                best.can=clust.can
                best.desc.arrive=desc.arrive
              }
            }
            if(bestdeltachi>0){
              go=TRUE
              curclust[d]=as.numeric(substring(best.can,4))
              curbin[,clust.d]=desc.parti
              curbin[,best.can]=best.desc.arrive
            }
          }
        }
      }
    }
  }
  #### end transfer
  
  retour=curclust
  names(retour)=colnames(data)[-1]
  return(list(clust=retour,criterion=mr.chisqNS(vec.cat,curbin)))
}