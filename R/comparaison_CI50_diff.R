comparaison_CI50_diff <- function(){
# Comparaison globale
# Listes pour stocker les objets pour chaque essai
pval.global <- list()
ci.test <- list()
conv.perm.global <- list()
conv.perm.pairs <- list()
# Pour chaque essai 
for (e in levels(data$essay)){
  # Données
  data.essay.e <- filter(data, essay == e)
  data.essay.e$essay <- factor(data.essay.e$essay)
  if (test.global.call){
    # Test F
    if (test.global == "test F"){
      pval.global[[e]] <- test_F(X=data.essay.e, EO=levels(data.essay.e$EO), pas=pas, bas0 = bas0)
      assign("pval.global", pval.global, envir = .GlobalEnv)
      
      # Test de permutation
    }else if(test.global == "test permutation"){
      test <- test_permutation(X=data.essay.e, EO=levels(data.essay.e$EO), pas=pas, B=B.perm, bas0 = bas0)
      assign("test", test, envir = .GlobalEnv)
      
      pval.global[[e]] <- test$pval
      assign("pval.global", pval.global, envir = .GlobalEnv)
      
      conv.perm.global[[e]] <- test$conv
      assign("conv.perm.global", conv.perm.global, envir = .GlobalEnv)
      
    }
    assign("data.essay.e", data.essay.e, envir = .GlobalEnv)
    
  }
}
assign("pval.global", pval.global, envir = .GlobalEnv)
assign("ci.test", ci.test, envir = .GlobalEnv)
assign("conv.perm.global", conv.perm.global, envir = .GlobalEnv)
assign("conv.perm.pairs", conv.perm.pairs, envir = .GlobalEnv)


# Vérification : pvalue pour la comparaison globale
if (test.global.call){
  if (any((is.na(pval.global)))){
    test.global.done <- FALSE
    assign("test.global.done", test.global.done, envir = .GlobalEnv)
    
    cat("Info : la comparaison globale n'a pas pu être réalisé\n")
  }else{
    test.global.done <- TRUE
    assign("test.global.done", test.global.done, envir = .GlobalEnv)
    
  }
}
  
# Comparaison par paires
# Listes pour stocker les objets pour chaque essai
pval.pairs <- list()
# Liste des paires
pairs <- expand.grid(levels(data$EO), levels(data$EO))
pairs <- t(apply(pairs, 1, sort))
pairs <- unique(pairs[pairs[,1] != pairs[,2], ])
assign("pairs", pairs, envir = .GlobalEnv)


# Nombre de pairs
nb.pairs <- nrow(pairs)
assign("nb.pairs", nb.pairs, envir = .GlobalEnv)

# Pour chaque essai 
for (e in levels(data$essay)){
  # Donn?es
  data.essay.e <- filter(data, essay == e)
  data.essay.e$essay <- factor(data.essay.e$essay)
  # Test F
  if (test.pairs == "test F"){
    # pvalues et convergence
    pval.pairs[[e]] <- rep(NA, nb.pairs)
    # Pour chaque paire :
    for (i in 1:nb.pairs){
      pval.pairs[[e]][i] <- test_F(X=data.essay.e, EO=pairs[i,], pas=pas, bas0=bas0)
    }
    assign("pval.pairs", pval.pairs, envir = .GlobalEnv)
    
    # Test de permutation
  }else if(test.pairs == "test permutation"){
    # pvalues et convergence
    pval.pairs[[e]] <- rep(NA, nb.pairs)
    conv.perm.pairs[[e]] <- rep(NA, nb.pairs)
    # Pour chaque paire :
    for (i in 1:nb.pairs){
      test <- test_permutation(X=data.essay.e, EO=pairs[i,], pas=pas, B=B.perm, bas0=bas0)
      pval.pairs[[e]][i] <- test$pval
      conv.perm.pairs[[e]][i] <- test$conv
    }
    assign("test", test, envir = .GlobalEnv)
    assign("pval.pairs", pval.pairs, envir = .GlobalEnv)
    assign("conv.perm.pairs", conv.perm.pairs, envir = .GlobalEnv)
    
    # Test avec intervalles de confiance
  }else if (test.pairs == "IC"){
    # calcul des IC pour les comparaisons par paires
    ci.test[[e]] <- distr.ci[[e]][, paste("logIC50", levels(data$EO), sep=".")]
    ci.test[[e]] <- t(apply(ci.test[[e]], 2, function(x) quantile(x, probs = c(risk/(2*nb.pairs), 1-risk/(2*nb.pairs)))))
    # Pour chaque paire :
    pval.pairs[[e]] <- rep(NA, nb.pairs)
    for (i in 1:nb.pairs){
      # intersection vide pour les deux IC ?
      ci.test.pairs <- ci.test[[e]][paste("logIC50", pairs[i, ], sep="."),]
      verif1 <- between(ci.test.pairs[1, ], ci.test.pairs[2, 1], ci.test.pairs[2, 2])
      verif2 <- between(ci.test.pairs[2, ], ci.test.pairs[1, 1], ci.test.pairs[1, 2])
      # pvalue
      if(!any(c(verif1, verif2))){
        pval.pairs[[e]][i] <- 0
      }else{
        pval.pairs[[e]][i] <- 1
      }
    }
    assign("ci.test", ci.test, envir = .GlobalEnv)
    assign("pval.pairs", pval.pairs, envir = .GlobalEnv)
    assign("ci.test.pairs", ci.test.pairs, envir = .GlobalEnv)
    assign("verif1", verif1, envir = .GlobalEnv)
    assign("verif2", verif2, envir = .GlobalEnv)
    
  }
  # pvalues
  names(pval.pairs[[e]]) <- paste(pairs[,1], pairs[,2], sep=".")
}

# V?rification : une pvalue pour chaque paire ?
if (any(is.na(unlist(pval.pairs)))){
  test.pairs.done <- FALSE
  assign("test.pairs.done", test.pairs.done, envir = .GlobalEnv)
  
  cat("Info : les comparaisons par paires n'ont pas pu toutes être réalisées\n")
}else{
  test.pairs.done <- TRUE
  assign("test.pairs.done", test.pairs.done, envir = .GlobalEnv)
  
}

# Convergence des tests de permutation
# stocké dans les objets conv.perm.global et conv.perm.pairs
}

