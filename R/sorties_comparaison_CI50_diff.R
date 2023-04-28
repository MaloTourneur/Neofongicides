LetterDisplay <- function(pvalues,alpha) {
  
  # Param?tres d'entr?e :
  # pvalues : matrice avec les valeurs p issues de la comparaison 
  #           par paires. Il suffit que la partie au dessus 
  #           de la diagonale soit renseign?e.
  # alpha : le risque alpha ajust?
  
  # Valeur en sortie :
  # une matrice avec une ligne par modalit? 
  # (dans le m?me ordre que dans pvalues)
  # et une colonne par lettre 
  # plus une colonne avec concat?nation des lettres
  
  n <- ncol(pvalues)
  
  # Indicator matrix : TRUE : same letter, not significant
  #                    FALSE : no letter in common, significant
  ind <- pvalues > alpha 
  
  clique <- NULL # matrix that will contain the cliques in its columns
  k <- 0 # number of cliques already created
  # Loop for the categories
  for (i in 1:n){
    # vector indicating if significance between i and smaller categories
    W <- ind[1:i,i]
    # If there is no "TRUE" in W, create a new column in clique;
    # Else try to insert i into existing cliques  
    if (sum(W,na.rm=TRUE)==0){
      k <- k+1
      clique <- cbind(clique,rep(0,n))
      clique[i,k] <- 1
    } else
    {
      m <- 1
      V <- rep(0,i)
      while (m <= k & sum(V==W,na.rm=TRUE)<(i-1)){
        # Is clique m a subset of W ?
        # That is, no index where clique is 1 but W is 0
        if (sum(clique[1:i,m] > W,na.rm=TRUE)==0) {
          # Put the found index into V :
          V[which(clique[1:i,m]==1)] <- 1
          # Add category i to the clique :
          clique[i,m] <- 1
        }  # End if
        m <- m+1
      } # End while
      W <- W - V
      # If there are 1s left in W, add new cliques
      if (sum(W,na.rm=TRUE)==1){
        # If there is just 1 category left, 
        # add a clique with this category
        k <- k+1
        clique <- cbind(clique,rep(0,n))
        clique[c(which(W==1),i),k] <- 1
      } else if (sum(W,na.rm=TRUE)>1) {
        # Else if more than 1 category is left, verify if all categories can be grouped together.
        a <- which(W==1)
        act <- 1 # Indicator if All Categories can be grouped Together
        for (i1 in 1:(length(a)-1)) {
          for (i2 in (i1+1):length(a)){
            if (!ind[a[i1],a[i2]]) judas <- 0 
            # If entry in ind is FALSE, the two categories cannot
            # be grouped together
          }  
        }
        # If all categories can be grouped together,
        # create just one new clique
        if (act) {
          k <- k+1
          clique <- cbind(clique,rep(0,n))
          clique[c(a,i),k] <- 1
        } else
        {
          # Otherwise create a new clique for each category
          for (i1 in 1:length(a)){
            k <- k+1
            clique <- cbind(clique,rep(0,n))
            clique[c(a[i1],i),k] <- 1
          } # End for
        } # End Else
      } # End else if (sum(W,na.rm=TRUE)>1)  
    } # End else  
  } # next i
  # Verify if the number of columns can be reduced
  # j <- 1 # the column to be tested
  # # A total of k verifications is made :
  # for (j3 in 1:k) {
  #   # Lines with 1's
  #   a <- which(clique[,j]==1)
  #   # Same lines of other columns with a subset?
  #   notfound <- 0 # If at least one pair is not found, the column is needed.
  #   if (length(a)==1) {
  #     if (sum(clique[a,-j])==0) notfound <- 1
  #   } else
  #   {
  #     # Verify pair by pair
  #     for (j1 in 1:(length(a)-1)){
  #       for (j2 in (j1+1):length(a)){
  #         if (max(colSums(clique[c(a[j1],a[j2]),-j]))<2) notfound<-1 
  #       }
  #     }
  #   }
  #     if (!notfound) {
  #       clique <- clique[,-j]
  #     } else 
  #       j <- j+1
  # } # Next j3 (next column)
  
  # Sweeping (reduce number of 1s) : not implemented yet
  
  # Transform clique into data.frame
  cliques <- data.frame(clique)
  row.names(cliques) <- rownames(ind)
  names(cliques) <- letters[1:k]
  
  # Replace the 1's by letters
  for (j in 1:k) {
    cliques[cliques[,j]==1,j] <- letters[j]
    cliques[cliques[,j]==0,j] <- ""
  }
  
  # New column with concatenation of letters
  cliques$groups <- cliques[,1]
  if (k > 1) {
    for (j in 2:k) {
      cliques$groups <- paste(cliques$groups,cliques[,j],sep="")
    }
  }
  return(cliques)
}


Table1_comparaison_CI50_diff <- function(){
  output <- paste(main.output, "comparaison_CI50", sep="/")
# Comparaison globale
if (test.global.call){
  if (test.global.done){
    # pvalue
    for (e in levels(data$essay)){
      pval.global[[e]] <- data.frame(essay=e, pvalue=pval.global[[e]])
    }
    pval.global <- do.call(rbind, c(pval.global, list(make.row.names=FALSE)))
    write.csv2(pval.global, file=paste(output, "comparaison_globale.csv", sep="/"), row.names=F)
  }
}
return(pval.global)
}

Table2_comparaison_CI50_diff <- function(){
  output <- paste(main.output, "comparaison_CI50", sep="/")
# Comparaisons par paires
if (test.pairs.done){
  tab.group <- list()
  for (e in levels(data$essay)){
    # Table pvalues (comparaisons par paires)
    tab.pval <- table(levels(data$EO), levels(data$EO))
    tab.pval <- tab.pval*NA
    for (i in 1:(nb.EO-1)){
      for (j in (i+1):nb.EO)
        tab.pval[i, j] <- pval.pairs[[e]][paste(levels(data$EO)[i], levels(data$EO)[j], sep=".")]
    }
    # Table groupes (comparaisons par paires)
    tab.group[[e]] <- LetterDisplay(tab.pval, risk/nb.pairs)
    tab.group[[e]] <- apply(as.matrix(tab.group[[e]][, -ncol(tab.group[[e]])]), 1, function(x) do.call(paste, c(as.list(x),list(sep=""))))
    tab.group[[e]] <- data.frame(essay=rep(e, nb.EO),
                                 EO=levels(data$EO),
                                 group=tab.group[[e]])

    if (test.pairs == "IC"){
      tab.group[[e]]$logIC50.inf=ci.test[[e]][, 1]
      tab.group[[e]]$logIC50.sup=ci.test[[e]][, 2]
    }
    colnames(tab.group[[e]]) <- gsub("logIC50", paste("logIC", prop, sep=""), colnames(tab.group[[e]]))
    tab.group[[e]] <- rbind(tab.group[[e]],rep(NA,5))
    assign("tab.group", tab.group, envir = .GlobalEnv)
  }
  tab.group <- do.call(rbind, c(tab.group, list(make.row.names=FALSE)))
  names(tab.group)[3] <- "group.defined.by.essay"
  assign("tab.group", tab.group, envir = .GlobalEnv)
  write.csv2(tab.group, file=paste(output, "comparaisons_paires.csv", sep="/"), na="", row.names=F)
}
  return(tab.group)
}