
#' AOV.Grappe  ANOVA et post-hoc analysis for Linear Mixed Models
#'
#' @description ANOVA table with F-tests and p-values using Satterthwaite's method for denominator
#' degrees-of-freedom and F-statistic. Models should be fitted with [lmer()] from the lmerTest-package.
#' Then  LS-means differences of LS-mean for all considered factors in the linear mixed model.
#'
#' @usage
#' AOV.Grappe(x,
#'  column,
#'   Model,
#'   Random = TRUE,
#'   randomodel=F,
#'   reducF=FALSE,
#'   reducR=FALSE,
#'   graphic=FALSE,
#'   verbose=TRUE)
#'
#' @param		x       dataframe
#' @param		column  number of the columns on which ANOVA are performed
#' @param	  Model   anova Model
#'										- Syntax for fixed factor				:	facteur_A
#'										- Syntax for Fixed factorsr			: 1|facteur_A)
#'										- Fixed factors				          : facteur1 + facteur2
#'										- Interaction for Fixed Factors	: facteur1 + facteur2 + facteur1:facteur2
#'										- Fixed factor & Fixed factor	  : facteur1 + (1|facteur2)
#'										- Interaction with random	      : (1|facteur1:facteur2)"
#' @param   Random Not use for the moment (to be check)
#' @param   randomodel Not use for the moment (to be check)
#' @param		reducF logical.  reduce fixed effect structure? FALSE by default.
#' @param	  reducR logical. reduce random effect structure? FALSE by default.
#' @param		graphic plot the graph TRUE / FALSE
#' @param   verbose logical.show the progression of the analysis ? TRUE by Default
#'
#' @import  utils lmerTest agricolae
#' @rawNamespace import(stats, except=step)
#'
#'
#' @examples
#'data(wine)
#'Var.Grappe(wine)
#'winef=Var.Grappe(wine,column = c(1:2), type="factor")
#'res.AOV <- AOV.Grappe( x=winef,column = c(3:4), "ProductName + (1|CJ)+ (1|ProductName:CJ)")
#'export.AOV(res.AOV)
#'library(RColorBrewer)
#'mycolor=brewer.pal(7,"Greens")
#'graph.AOV(res.AOV,color.graph=mycolor)
#'graph.AOV(res.AOV,title.graph="Sensory Score", add.moy=TRUE, x.ordered=1, ymin=1, ymax=9)
#'
#'
#'
#' @export
AOV.Grappe <- function(x, column, Model, Random = TRUE, randomodel=FALSE,
                         reducF=FALSE, reducR=FALSE, graphic=FALSE, verbose=TRUE)
{
  #Prepare les objets pour stocker les resultats
  #----------------------------------------------+
  #Nombre de variables a analyser
  attach(x)
  nbvar <- length(column)
  #Modele
  mod.temp <- formula(paste("variable", Model, sep="~"))
  #Matrice de base pour regrouper les p.values, les etoiles et les F
  mat.base <- matrix(data=NA, nrow=length(attr(terms(mod.temp), "term.labels")), ncol=nbvar)
  colnam <- NULL
  for(n in 1:nbvar) { colnam[n] <- colnames(x[column])[n] }
  colnames(mat.base) <- colnam


  #Liste regroupant tous les sous-objets de resultats (pvalues + posthocs)
  Total.result <- list()
  Total.result <- c(Total.result, list(pvalue=mat.base, signif=mat.base, fvalue=mat.base,  lsd.result=NULL))


  #Identification des Facteurs Fixes a etudier en fonction de la presence ou non d'effet fixe
  initcol =column[1]
  nom_var <- names(x)[initcol]
  if (Random==FALSE){
    formula=as.formula(paste(nom_var, Model, sep="~"))
    Fact = attr(terms(formula),"term.labels")
  }	else
  {
    variable <- x[, initcol]
    res.base <- lmer(formula=formula(paste(nom_var, Model, sep="~")), data=x)
    lsmeans.table <-as.data.frame(ls_means(res.base))
    lsmeans.table$term=as.factor(lsmeans.table$term)
    Fact=levels(lsmeans.table$term)}


  #Listes et sous listes dans l'objet final pour chaque facteurs dans les post-hoc

  for(f in 1:length(Fact))
  {
    #Nombre de niveaux par facteur
    verif.inter <- length(unlist(strsplit(Fact[f], split=":", fixed=TRUE)))
    if(verif.inter == 1)
    {
      #Si le facteur est simple
      nlev <- nlevels(x[, Fact[f]])
      row.names <- levels(x[, Fact[f]])
    } else
    {
      #Si le facteur est une interaction
      ntp <- unlist(strsplit(Fact[f], split=":", fixed=TRUE))
      nlev <- nlevels(x[, ntp[1]])*nlevels(x[, ntp[2]])
      row.names <- NULL
      rcount <- 1
      for(n in 1:nlevels(x[, ntp[1]]))
      {
        n1 <- levels(x[, ntp[1]])[n]
        for(p in 1:nlevels(x[, ntp[2]]))
        {
          n2 <- levels(x[, ntp[2]])[p]
          row.names[rcount] <- paste(n1, n2, sep=" - ")
          rcount <- rcount + 1
        }
      }
    }
    #Prepare le tableau de resultat pour le LSD
    lsd.res <- as.data.frame(matrix(data=NA, nrow=nlev, ncol=0))
    rownames(lsd.res) <- row.names
    #Regroupe tout dans l'objet final
    Total.result$lsd.result <- c(Total.result$lsd.result, list(lsd.res))
    names(Total.result$lsd.result)[[f]] <- Fact[f]
  }


  #Compteur pour remplir les matrices
  ctm <- 0
  #Vecteur pour les lettres dans les tests post-hoc
  Lettre <- paste(c(letters, LETTERS, 1:9,letters))


  for(A in column)
  {



    #Incrementation du compteur
    ctm <- ctm + 1

    #Extrait les donnees du tableau source
    nom_var <- names(x)[A]
    variable <- x[, A]
    if(verbose == TRUE) { cat("\n","\nVARIABLE :", toupper(nom_var), "\n")}
    res.base <- lmer(formula=formula(paste(nom_var, Model, sep="~")), data=x)
    res.step <- lmerTest::step(res.base, reduce.fixed=reducF, reduce.random=reducR)
    final_fm <- get_model(res.step)
    res.step.final<-anova(final_fm)

    resf=res.step$fixed
    resr=res.step$random

    if (reducF==F) {
      #Extrait les pvalues - Effets Fixed
      pv=res.step.final["Pr(>F)"]
      pv <- subset(pv, rownames(pv)!="Residuals")
      pv <- subset(pv, rownames(pv)!="<none>")
      colnames(pv) <- "p.value"
    }  else {
      pv <- resf["Pr(>F)"]
      pv <- subset(pv, rownames(pv)!="Residuals")
      pv <- subset(pv, rownames(pv)!="<none>")
      colnames(pv) <- "p.value"}

    #for (p in length(rownames(pvf)))
    #{pv <- subset(pv, rownames(pv)!=rownames(pvf)[p])}
    # pv<-unique(rbind(pv,pvf))


    #Extrait les pvalues - Effets randoms
    pv1 <- resr["Pr(>Chisq)"]
    pv1 <- subset(pv1, rownames(pv1)!="Residuals")
    pv1 <- subset(pv1, rownames(pv1)!="<none>")
    colnames(pv1) <- "p.value"



    #Enregistre le tout dans l'objet pvalue
    #pvalues
    rp <- rbind(pv, pv1)
    Total.result$pvalue[, ctm] <- round(rp[order(rownames(rp), decreasing=F), ], 4)
    rownames(Total.result$pvalue) <- sort(rownames(rp))

    #Transforme les p.values en etoile

    Total.result$signif[, ctm] <- STAR.Grappe(Total.result$pvalue[, ctm])
    rownames(Total.result$signif) <- sort(rownames(rp))

    #F value
    #Les noms de colonnes ne sont pas identiques pour les F
    #La variable fcol permet de resoudre ce problème
    fcol <- pmatch(x=c("F.value", "F value"), names(resf), nomatch=0)
    if(fcol[1] == 0) { fname <- "F value" } else { fname <- "F.value" }
    fcol <- fcol[1] + fcol[2]
    ##Extrait les fvalues - Effets fixes

    if (reducF==F) {
      fv=res.step.final["F value"]
      fv <- subset(fv, rownames(fv)!="Residuals")
      fv <- subset(fv, rownames(fv)!="<none>")
    }  else {
      fv <- resf["F value"]
      fv <- subset(fv, rownames(fv)!="Residuals")
      fv <- subset(fv, rownames(fv)!="<none>")}


    # Extrait le Chi² - Effets randoms
    cf <- resr["LRT"]
    cf <- subset(cf, rownames(cf)!="Residuals")
    cf <- subset(cf, rownames(cf)!="<none>")
    colnames(cf) <- fname


    #Enregistre le tout dans l'objet fvalue
    rf <- rbind(fv, cf)
    Total.result$fvalue[, ctm] <- round(rf[order(rownames(rf), decreasing=F), ], 3)

    #Nom de lignes
    rownames(Total.result$fvalue) <- sort(rownames(rf))

    res.base.lsmeans.table <-as.data.frame(ls_means(res.base))
    res.base.difflsmeans.table<-as.data.frame(difflsmeans(res.base))
    res.base.lsmeans.table$term=as.factor(res.base.lsmeans.table$term)
    res.base.lsmeans.table$levels=as.factor(res.base.lsmeans.table$levels)
    Fact.base = levels(res.base.lsmeans.table$term)
    res.step.lsmeans.table <-as.data.frame(ls_means(final_fm))
    res.step.difflsmeans.table<-as.data.frame(difflsmeans(final_fm))
    res.step.difflsmeans.table$term=as.factor(res.step.difflsmeans.table$term)
    res.step.difflsmeans.table$levels=as.factor(res.step.difflsmeans.table$levels)
    res.step.lsmeans.table$term=as.factor(res.step.lsmeans.table$term)
    res.step.lsmeans.table$levels=as.factor(res.step.lsmeans.table$levels)


    ######################################
    # Boucle pour les tests posthocs - LSD
    #-------------------------------------+
    for(ph in c(1:length(Fact)))
    {
      #Extrait le facteur d'?tude
      facteur <- Fact[ph]
      if(verbose == TRUE) { cat("\nFacteur  :", facteur)}
      verif.signi <- as.matrix(Total.result$pvalue)[facteur, nom_var]

      if(verif.signi > 0.05)
      { sou <- res.base.difflsmeans.table[res.base.difflsmeans.table$term==facteur,]
      long <-length(rownames((res.base.lsmeans.table[res.base.lsmeans.table$term==facteur,])))
      nam <- as.character(res.base.lsmeans.table[res.base.lsmeans.table$term==facteur,]$levels)
      sou1<-as.data.frame(res.base.lsmeans.table)
      } else if(randomodel!=TRUE)
      {sou <- res.step.difflsmeans.table[res.step.difflsmeans.table$term==facteur,]
      long <-length(rownames((res.step.lsmeans.table[res.step.lsmeans.table$term==facteur,])))
      nam <- as.character(res.step.lsmeans.table[res.step.lsmeans.table$term==facteur,]$levels)
      sou1<-as.data.frame(res.step.lsmeans.table)
      }    else
      {
        sou <- res.base.difflsmeans.table[res.base.difflsmeans.table$term==facteur,]
        long <-length(rownames((res.base.lsmeans.table[res.base.lsmeans.table$term==facteur,])))
        nam <- as.character(res.base.lsmeans.table[res.base.lsmeans.table$term==facteur,]$levels)
        sou1<-as.data.frame(res.base.lsmeans.table)
      }


      #Tableau de donnees
      names(sou)[9] <- "p-value"
      #Extrait les noms de chaque binôme teste
      while(length(grep("  ", rownames(sou))) != 0) { rownames(sou) <- gsub("  ", " ", rownames(sou)) }
      nom <- gsub(facteur, "", sou$levels)
      sou$n1 <- gsub(" -.*", "", nom)
      sou$n2 <- gsub(".*- ", "", nom)
      #Ordonne le tableau selon les p-values et l'affiche
      sou <- sou[order(sou[, "p-value"], decreasing=FALSE), ]

      #######XXX

      #Cree et affiche la matrice finale
      mat <- matrix("a", ncol=5, nrow=long, dimnames=list(1:long, c(facteur, "Grp", nom_var, "Std_err", "TMP")))
      #Ajoute les noms en colonne 1
      mat[, 1] <- nam

      #Estimate
      mat[, 3] <-  round(sou1$"Estimate"[sou1$term==facteur],3)
      #Standard Error
      mat[, 4] <- sou1$"Std. Error"[sou1$term==facteur]

      mat <-  mat[order(mat[, nom_var], decreasing = FALSE), ]

      #Boucle 1 : Assigne une lettre differente aux modalites signif. differentes
      #---------+
      for(l in 1:length(sou$n1))
      {
        #Boucle qui ne s'applique qu'aux moyennes significativement differentes
        #Dans X - Y : Si Estimate < 0, alors X < Y
        #Il faut donc que le groupe de Y soit "superieur" : X groupe a et Y groupe b

        if(sou$"p-value"[l] <= 0.05 && sou$Estimate[l] < 0)
        {
          #Pour chaque modalite, trouve la ligne correspondante dans la matrice
          if(pmatch(sou$n2[l], mat[, 1], nomatch=0) > 0)
          {
            ligne <- pmatch(sou$n2[l], mat[, 1], nomatch=0)
          } else {
            ligne <- long
          }
          #Extrait le numero de la lettre (a=1, b=2, etc.)
          lettre <- pmatch(mat[ligne, 2], Lettre, nomatch=0)
          #Remplace la lettre initiale par la suivante dans l'alphabet (groupe different)
          mat[ligne, 2] <- Lettre[lettre+1]
        } else
        {
          #Dans X - Y : Si Estimate > 0, alors X > Y
          #Il faut donc que le groupe de X soit "superieur" : X groupe b et Y groupe a
          if(sou$"p-value"[l] <= 0.05 && sou$Estimate[l] > 0)
          {
            #Pour chaque modalite, trouve la ligne correspondante dans la matrice
            if(pmatch(sou$n1[l], mat[, 1], nomatch=0) > 0)
            {
              ligne <- pmatch(sou$n1[l], mat[, 1], nomatch=0)
            } else {
              ligne <- long
            }
            #Extrait le numero de la lettre (a=1, b=2, etc.)
            lettre <- pmatch(mat[ligne, 2], Lettre, nomatch=0)
            #Remplace la lettre initiale par la suivante dans l'alphabet (groupe different)
            mat[ligne, 2] <- Lettre[lettre+1]
          }
        }
      }

      #Corrections des groupes
      #-----------------------+
      #La boucle 1 genère parfois des groupes trop "eloignes" (car procede par iterations)
      #Par exemple, la boucle retourne : a, a, b, c, e, f (lettres non  consecutives)
      #La correction permet d'obtenir  : a, a, b, c, d, e (lettres bien consecutives)
      #-----------------------------------------------------------------------------------+
      #Tri la matrice par nom de groupe croissant
      mapping<-data.frame(avant=Lettre,apres=c(11:97))
      mat[,5]<-mapping$apres[match(mat[,2],mapping$avant)]
      mat[,5]=as.integer( mat[,5])
      mat <-  mat[order(mat[, "TMP"], decreasing = FALSE), ]


      #Boucle  pour corriger chaque ligne : comparaisons 2 a 2 (1/2 ; 2/3 ; etc.)
      v <- 1

      while(v <= (length(mat[, 2])-1))
      {
        #Extrait les lettres des lignes x et x+1 pour les comparer
        lettre1 <- mat[v, 2]
        lettre2 <- mat[v+1, 2]
        #Compare les 2 lettres via leur position dans l'alphabet
        #Si 2 lettres identiques ou consecutives :
        ## - Alors difference de position < ou = 1
        ## - Sinon difference de position > 1
        if((pmatch(lettre2, Lettre, nomatch=0) - pmatch(lettre1, Lettre, nomatch=0)) > 1)
        {
          #Corrige la lettre trop "eloignee" (non consecutive)
          lv <- v
          #Boucle pour corriger toutes les lettres jusqu'a la fin de la matrice
          while(lv < length(mat[, 2]))
          {
            #Remplace la lettre trop eloignee par la precedente dans l'alphabet
            mat[lv+1, 2] <- Lettre[pmatch(mat[lv+1, 2], Lettre)-1]
            lv <- lv+1
          }
          #Incremente le compteur
          ## Pour reprendre a la ligne precedant la dernière modification
          ## -2 car il sera compense par le +1 plus loin (-2 +1 = -1 : donc 1 ligne avant)
          v <- v-2
        }

        v <- v+1
      }

      #Boucle 2 : Prend en compte les effets non significatifs
      #---------+
      #Certains effets non significatifs engendrent des groupes "multiples" du genre ab, bcd, etc.
      #La boucle 2 permet d'ajuster les groupes pour en tenir compte
      #-------------------------------------------------------------------------------------------+
      #Tri le tableau source en fonction du premier nom des modalites par ordre croissant
      sou <- sou[order(sou[, "n1"], decreasing=FALSE), ]
      #Boucle pour chaque ligne du tableau


      for(l in 1:length(sou$n1))
      {
        #Ne s'applique qu'aux differences non significatives ==> Donc pas de groupe different
        if(sou$"p-value"[l] > 0.05)
        {
          #Pour chaque modalite, trouve la ligne correspondante dans la matrice
          ligne1 <- pmatch(sou$n1[l], mat[, 1], nomatch=0)
          ligne2 <- pmatch(sou$n2[l], mat[, 1], nomatch=0)
          #Extrait les lettres
          lettre1 <- unlist(strsplit(x=mat[ligne1, 2], split=""))
          lettre2 <- unlist(strsplit(x=mat[ligne2, 2], split=""))
          #Verifie s'il existe au moins un groupe commun entre les modalites
          ##S'il en existe au moins 1, la somme sera differente de 0
          verif <- sum(match(lettre1, lettre2), na.rm=TRUE)
          #Si pas groupe commun, alors qu'il devrait en avoir 1 : verif = 0
          if(verif == 0)
          {
            #Compare les dernières lettres des groupes de chaque modalites
            #Si Modalite 1 < Moldalite 2
            if(pmatch(tail(lettre1, 1), Lettre, nomatch=0) < pmatch(tail(lettre2, 1), Lettre, nomatch=0))
            {
              #Ajoute a la 1ère modalite la 1ère lettre de la 2ème modalite
              new_name <- Lettre[pmatch(lettre1[1], Lettre, nomatch=0):pmatch(lettre2[1], Lettre, nomatch=0)]
              nName <- NULL
              for(nn in 1:length(new_name)) { nName <- paste(nName, new_name[nn], sep="") }
              mat[ligne1, 2] <- nName
            } else {
              #Si Modalite 1 > Moldalite 2
              if(pmatch(tail(lettre1, 1), Lettre, nomatch=0) > pmatch(tail(lettre2, 1), Lettre, nomatch=0))
              {
                #Ajoute a la 2ème modalite la 1ère lettre de la 1ère modalite
                new_name <- Lettre[pmatch(lettre2[1], Lettre, nomatch=0):pmatch(lettre1[1], Lettre, nomatch=0)]
                nName <- NULL
                for(nn in 1:length(new_name)) { nName <- paste(nName, new_name[nn], sep="") }
                mat[ligne2, 2] <- nName
              }
            }
          }
        }
      }
      mat <- mat[order(mat[, facteur], decreasing=FALSE), ]
      #Pour rire : ajoute les espace pour aligner les lettres similaires (NE PAS SUPPRIMER)
      for(lol in 1:length(mat[, 2]))
      {
        lol_lettre <- head(unlist(strsplit(x=(mat[lol, 2]), split="")), 1)
        lol_positi <- pmatch(lol_lettre, Lettre, nomatch=1)
        lol_nn <- rep(" ", lol_positi-1)
        lol_NewNom <- NULL
        for(ll in 0:length(lol_nn)) { lol_NewNom <- paste(lol_NewNom, lol_nn[ll], sep="") }
        mat[lol, 2] <- paste(lol_NewNom, mat[lol, 2], sep="")
      }
      #Arrondi les valeurs
      mat[, c(3, 4)] <- round(as.numeric(mat[, c(3, 4)]), 3)
      grp <- gsub(pattern=" ", replacement="", x=mat[, 2])
      #Enregistre les resultats
      Total.result$lsd.result[[facteur]] <- cbind(Total.result$lsd.result[[facteur]], mat[, c(3, 4, 2)])
    }
    if(verbose == TRUE) { cat("\n")}

  }
  detach(x)
  return(Total.result) }

