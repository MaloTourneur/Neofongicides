# Fonction logistique à 4 paramètres, avec bas fixé à 0
# Arguments :
# - x : valeurs (nombre ou vecteur)
# - par1 : haut
# - par3 : logarithme décimal du point d'inflexion
# - par4 : pente
# Retourne : un nombre ou un vecteur contenant la ou les images de x par la fonction

logistic4_bottom0 <- function(x,par1,par3,par4){
  y <- x*10^(-par3)
  value <- 0 + (par1-0)/(1+((prop/100)/(1-prop/100))*y^par4)
  return(value)
}


# Fonction logistique ? 4 param?tres
# Arguments :
# - x : valeurs (nombre ou vecteur)
# - par1 : haut
# - par2 : bas
# - par3 : logarithme d?cimal du point d'inflexion
# - par4 : pente
# Retourne : un nombre ou un vecteur contenant la ou les images de x par la fonction

logistic4 <- function(x,par1,par2,par3,par4){
  y <- x*10^(-par3)
  value <- par2 + (par1-par2)/(1+((prop/100)/(1-prop/100))*y^par4)
  return(value)
}



# Fonction qui utilise un test F pour comparer les logCIp de plusieurs huiles essentielles
# Arguments :
# - X : les données sur lesquelles le modèle a été ajusté
# - EO : vecteur contenant les huiles essentielles qu'on veut comparer
# - pas : le pas pour la grille de logCIp initiales testés pour optimiser les paramètres initiaux
#         avant l'ajustement du modèle
# Retourne : la p-valeur obtenue ainsi qu'un renseignement sur un éventuel problème de convergence

test_F <- function(X,EO,pas,maxiter=50,bas0=FALSE){
  # Nouvelle table pour ajuster les modèles requis (restreinte aux huiles essentielles concernées)
  if (length(levels(X$EO))!=length(EO)){
    Z <- X[X$EO %in% EO,]
    Z$EO <- factor(Z$EO)
  }else{
    Z <- X
  }
  # Si bas n'est pas fixé à 0
  if (bas0 == FALSE){
    # Noms des paramètres / initialisation
    par <- c("top","bottom","logIC50","slope")
    init <- c(max(Z$OD),min(Z$OD),0,1)
    names(init) <- par
    pval <- NA
    tryCatch({ # Gestion des problèmes de convergence
      # Modèle non partagé
      model.non.partage <- nlse_lite(f="logistic4", 
                                     init=init, data=Z, x='conc', y='OD',
                                     groups=T, groups.col = c('EO', 'essay'),
                                     groups.par = list(par, par[c(1, 2, 4)]),
                                     call=T, result=F, maxiter=maxiter)
      init.new <- modif_init(model.non.partage$args$start, Z, pas = pas)
      model.non.partage$args$start <- init.new
      model.non.partage <- do.call(model.non.partage$optimiser, model.non.partage$args)
      
      # Modèle partagé
      model.partage <- nlse_lite(f="logistic4", 
                                 init=init, data=Z, x='conc', y='OD',
                                 groups=T, groups.col = c('EO', 'essay'),
                                 groups.par = list(par[c(1,2,4)], par[c(1,2,4)]),
                                 call=T, result=F, maxiter=maxiter)
      init.new <- modif_init(model.partage$args$start, Z, pas = pas)
      model.partage$args$start <- init.new
      # model.partage$args$start["logIC50"] <- mean(init.new)
      
      model.partage <- do.call(model.partage$optimiser,model.partage$args)
      # Calcul de la p-valeur
      aov <- anova(model.partage,model.non.partage)
      pval <- aov$`Pr(>F)`[2]
    },
    error = function(e){
      return(NULL)
    })
  }else{
    # Si bas est fixé à 0
    # Noms des paramètres / initialisation
    par <- c("top","logIC50","slope")
    init <- c(max(Z$OD),0,1)
    names(init) <- par
    pval <- NA
    tryCatch({ # Gestion des problèmes de convergence
      # Modèle non partagé
      model.non.partage <- nlse_lite(f="logistic4_bottom0", 
                                     init=init, data=Z, x='conc', y='OD',
                                     groups=T, groups.col = c('EO', 'essay'),
                                     groups.par = list(par, par[c(1, 3)]),
                                     call=T, result=F, maxiter=maxiter)
      init.new <- modif_init(model.non.partage$args$start, Z, pas = pas,bas0=bas0)
      model.non.partage$args$start <- init.new
      model.non.partage <- do.call(model.non.partage$optimiser, model.non.partage$args)
      
      # Modèle partagé
      model.partage <- nlse_lite(f="logistic4_bottom0", 
                                 init=init, data=Z, x='conc', y='OD',
                                 groups=T, groups.col = c('EO', 'essay'),
                                 groups.par = list(par[c(1,3)], par[c(1,3)]),
                                 call=T, result=F, maxiter=maxiter)
      init.new <- modif_init(model.partage$args$start, Z, pas = pas,bas0=bas0)
      model.partage$args$start <- init.new
      # model.partage$args$start["logIC50"] <- mean(init.new)
      
      model.partage <- do.call(model.partage$optimiser,model.partage$args)
      # Calcul de la p-valeur
      aov <- anova(model.partage,model.non.partage)
      pval <- aov$`Pr(>F)`[2]
    },
    error = function(e){
      return(NULL)
    })    
  }
  
  return(pval)
}




comparaison_CI50_commune <- function(){
# Comparaison globale
if (test.global.call){
  # Test F
  if (test.global == "test F"){
    pval.global <- test_F(X=data, EO=levels(data$EO), pas=pas,bas0=bas0)
    assign("pval.global", pval.global, envir = .GlobalEnv)
    
  # Test de permutation
  }else if(test.global == "test permutation"){
    test <- test_permutation(X=data, EO=levels(data$EO), pas=pas, B=B.perm,bas0=bas0)
    assign("test", test, envir = .GlobalEnv)
    
    pval.global <- test$pval
    assign("pval.global", pval.global, envir = .GlobalEnv)
    
    conv.perm.global <- test$conv
    assign("conv.perm.global", conv.perm.global, envir = .GlobalEnv)
    
  }
}

# Vérification : pvalue pour la comparaison globale
if (test.global.call){
  if ((is.na(pval.global))){
    test.global.done <- FALSE
    assign("test.global.done", test.global.done, envir = .GlobalEnv)
    
    cat("Info : la comparaison globale n'a pas pu être réalisé\n")
  }else{
    test.global.done <- TRUE
    assign("test.global.done", test.global.done, envir = .GlobalEnv)
    
  }
}


# Comparaison par paires
# Liste des paires
pairs <- expand.grid(levels(data$EO), levels(data$EO))
pairs <- t(apply(pairs, 1, sort))
pairs <- unique(pairs[pairs[,1] != pairs[,2], ])
assign("pairs", pairs, envir = .GlobalEnv)

# Nombre de pairs
nb.pairs <- nrow(pairs)
assign("nb.pairs", nb.pairs, envir = .GlobalEnv)

# Test F
if (test.pairs == "test F"){
  # pvalues et convergence
  pval.pairs <- rep(NA, nb.pairs)
  # Pour chaque paire :
  for (i in 1:nb.pairs){
    pval.pairs[i] <- test_F(X=data, EO=pairs[i,], pas=pas,bas0=bas0)
  }
  assign("pval.pairs", pval.pairs, envir = .GlobalEnv)
  
# Test de permutation
}else if(test.pairs == "test permutation"){
  # pvalues et convergence
  pval.pairs <- rep(NA, nb.pairs)
  conv.perm.global <- rep(NA, nb.pairs)
  # Pour chaque paire :
  for (i in 1:nb.pairs){
    test <- test_permutation(X=data, EO=pairs[i,], pas=pas, B=B.perm,bas0=bas0)
    pval.pairs[i] <- test$pval
    conv.perm.global[i] <- test$conv
  }
  assign("conv.perm.global", conv.perm.global, envir = .GlobalEnv)
  assign("test", test, envir = .GlobalEnv)
  assign("pval.pairs", pval.pairs, envir = .GlobalEnv)
  # Test avec intervalles de confiance
}else if (test.pairs == "IC"){
  #colnames(distr.ci) <- names(model$args$start)
  # calcul des IC pour les comparaisons par paires
  ci.test <- distr.ci[, paste("logIC50", levels(data$EO), sep=".")]
  ci.test <- t(apply(ci.test, 2, function(x) quantile(x, probs = c(risk/(2*nb.pairs), 1-risk/(2*nb.pairs)))))
  # Pour chaque paire :
  pval.pairs <- rep(NA, nb.pairs)
  for (i in 1:nb.pairs){
    # intersection vide pour eles deux IC ?
    ci.test.pairs <- ci.test[paste("logIC50", pairs[i, ], sep="."),]
    verif1 <- between(ci.test.pairs[1, ], ci.test.pairs[2, 1], ci.test.pairs[2, 2])
    verif2 <- between(ci.test.pairs[2, ], ci.test.pairs[1, 1], ci.test.pairs[1, 2])
    # pvalue
    if(!any(c(verif1, verif2))){
      pval.pairs[i] <- 0
    }else{
      pval.pairs[i] <- 1
    }
  }
  assign("ci.test", ci.test, envir = .GlobalEnv)
  assign("pval.pairs", pval.pairs, envir = .GlobalEnv)
  assign("ci.test.pairs", pval.pairs, envir = .GlobalEnv)
  assign("verif1", pval.pairs, envir = .GlobalEnv)
  assign("verif2", pval.pairs, envir = .GlobalEnv)
  
}
# pvalues
names(pval.pairs) <- paste(pairs[,1], pairs[,2], sep=".")

# Vérification : une pvalue pour chaque paire ?
if (any(is.na(pval.pairs))){
  test.pairs.done <- FALSE
  assign("test.pairs.done", test.pairs.done, envir = .GlobalEnv)
  
  cat("Info : les comparaisons par paires n'ont pas pu toutes être réalisées\n")
}else{
  test.pairs.done <- TRUE
  assign("test.pairs.done", test.pairs.done, envir = .GlobalEnv)
  
}

# Congergence des tests de permutation
# stocké dans les objets conv.perm.global et conv.perm.pairs
}
