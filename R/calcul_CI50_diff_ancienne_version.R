# Paramètres
par <- c("top", "bottom", "logIC50", "slope")

# Listes pour stocker différents objets pour chaque essai
data.essay <- list()
residuals <- list()
estimation <- list()
ci <- list()
distr.ci <- list()
conv.ci <- list()

# Pour chaque essai 
for (e in levels(data$essay)){
  
  # Choix données par essai
  data.essay[[e]] <- filter(data, essay == e)
  data.essay[[e]]$essay <- factor(data.essay[[e]]$essay)
  
  # Initialisation des paramètres
  init <- c(max(data.essay[[e]]$OD), min(data.essay[[e]]$OD), 0, 1)
  names(init) <- par
  
  # Modèle 
  model <- nlse_lite(data=data.essay[[e]],
                     f="logistic4", 
                     init=init,
                     x='conc',
                     y='OD',
                     groups=T,
                     groups.col = c('EO', 'essay'),
                     groups.par = list(par, par[c(1, 2, 4)]),
                     call=T,
                     result=F,
                     maxiter = maxiter)
  
  # Pas pour la grille de logCI50 possibles             
  a <- floor(log10(min(data.essay[[e]]$conc[data.essay[[e]]$conc>0])))
  b <- ceiling(log10(max(data.essay[[e]]$conc)))
  pas <- seq(a, b, length.out = taille.grille)[2] - seq(a, b, length.out = taille.grille)[1]
  # Optimisation des valeurs initales pour les logCI50
  init.new <- modif_init(model$args$start, data.essay[[e]], pas = pas)
  model$args$start <- init.new
  
  # Estimation
  estimation[[e]] <- do.call(model$optimiser, model$args)
  # Sauvegarde des résidus
  residuals[[e]] <- estimation[[e]]$m$resid()
  estimation[[e]] <- summary(estimation[[e]])$coefficients[,1]
  for (comp in levels(data.essay[[e]]$EO)){
    estimation[[e]][paste("logIC50", comp, e, sep=".")] <- estimation[[e]][paste("logIC50", comp, sep=".")]
    estimation[[e]] <- estimation[[e]][names(estimation[[e]]) != paste("logIC50", comp, sep=".")]
  }
  
  # Intervalles de confiance
  if (ci.method == "bootstrap"){
    ci[[e]] <- ic_bootstrap(model, names(model$args$start), B=B.boot, level=lvl)
    conv.ci[[e]] <- ci[[e]]$conv
    distr.ci[[e]] <- ci[[e]]$distr
    colnames(distr.ci[[e]]) <- names(model$args$start)
    ci[[e]] <- ci[[e]]$ci
  }else if(ci.method == "Wald"){
    ci[[e]] <- confint2(do.call(model$optimiser,model$args))
  }
  for (comp in levels(data.essay[[e]]$EO)){
    ci[[e]] <- rbind(ci[[e]][paste("logIC50", comp, sep="."), ], ci[[e]])
    rownames(ci[[e]])[1] <- paste("logIC50", comp, e, sep=".")
    ci[[e]] <- ci[[e]][rownames(ci[[e]]) != paste("logIC50", comp, sep="."),]
  }
  
}

# Regroupement des objets pour les différents essais
data.essay <- do.call(rbind, c(data.essay, list(make.row.names=F)))
residuals <- do.call(c, c(residuals, list(use.names=F)))
names(estimation) <- NULL
estimation <- do.call(c, estimation)
ci <- do.call(rbind, ci)
conv.ci <- mean(unlist(conv.ci))

# Vérification taux de convergence IC (bootstrap)
IC.done <- TRUE
if(ci.method == "bootstrap"){
  if((conv.ci/B.boot < conv.ci.lvl)){
    IC.done <- FALSE
    cat("Info : le taux de convergence pour les intervalles de confiance est insatisfaisant\n")
  }
}



