calcul_CI50_diff <- function(data, bas0, taille.grille, nb.essay, B.boot, lvl, conv.ci.lvl){
  a <- floor(log10(min(data$conc[data$conc>0])))
  assign("a", a, envir = .GlobalEnv)
  
  b <- ceiling(log10(max(data$conc)))
  assign("b", b, envir = .GlobalEnv)
  
  
# Paramètres
if (bas0 == TRUE){
  par <- c("top", "logIC50", "slope")
  assign("par", par, envir = .GlobalEnv)
} else{
  par <- c("top", "bottom", "logIC50", "slope")
  assign("par", par, envir = .GlobalEnv)
}

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
  if (bas0 == TRUE){
    init <- c(max(data.essay[[e]]$OD), 0, 1)
    assign("init", init, envir = .GlobalEnv)
  } else{
    init <- c(max(data.essay[[e]]$OD), min(data.essay[[e]]$OD), 0, 1)
    assign("init", init, envir = .GlobalEnv)
  }
  names(init) <- par
  assign("names(init)", par, envir = .GlobalEnv)
  
  # Modèle 
  if (bas0 == TRUE){
    model <- nlse_lite(data=data.essay[[e]],
                       f="logistic4_bottom0", 
                       init=init,
                       x='conc',
                       y='OD',
                       groups=T,
                       groups.col = c('EO', 'essay'),
                       groups.par = list(par, par[c(1, 3)]),
                       call=T,
                       result=F,
                       maxiter = maxiter)
    assign("model", model, envir = .GlobalEnv)
    
  } else{
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
    assign("model", model, envir = .GlobalEnv)
    
  }
  
  # Pas pour la grille de logCI50 possibles             
  a <- floor(log10(min(data.essay[[e]]$conc[data.essay[[e]]$conc>0])))
  b <- ceiling(log10(max(data.essay[[e]]$conc)))
  pas <- seq(a, b, length.out = taille.grille)[2] - seq(a, b, length.out = taille.grille)[1]
  # Optimisation des valeurs initales pour les logCI50
  init.new <- modif_init(model$args$start, data.essay[[e]], pas = pas, bas0=bas0)
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
  
} # Fin for (e in levels(data$essay))

# Regroupement des objets pour les différents essais
data.essay <- do.call(rbind, c(data.essay, list(make.row.names=F)))
residuals <- do.call(c, c(residuals, list(use.names=F)))
names(estimation) <- NULL
estimation <- do.call(c, estimation)
ci <- do.call(rbind, ci)
conv.ci <- mean(unlist(conv.ci))


assign("data.essay", data.essay, envir = .GlobalEnv)
assign("names(estimation)", NULL, envir = .GlobalEnv)
assign("residuals", residuals, envir = .GlobalEnv)
assign("estimation", estimation, envir = .GlobalEnv)
assign("conv.ci", conv.ci, envir = .GlobalEnv)
assign("distr.ci", distr.ci, envir = .GlobalEnv)
assign("ci", ci, envir = .GlobalEnv)

# Vérification taux de convergence IC (bootstrap)
IC.done <- TRUE
assign("IC.done", IC.done, envir = .GlobalEnv)

if(ci.method == "bootstrap"){
  if((conv.ci/B.boot < conv.ci.lvl)){
    IC.done <- FALSE
    assign("IC.done", IC.done, envir = .GlobalEnv)
    cat("Info : le taux de convergence pour les intervalles de confiance est insatisfaisant\n")
  }
}

}