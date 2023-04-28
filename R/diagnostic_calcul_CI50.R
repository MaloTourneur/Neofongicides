diagnostic_calcul_CI50 <- function(){
# Choix possibles pour les couples manip / produit
choice <- expand.grid(essay=levels(data$essay), EO=levels(data$EO))
assign("choice", choice, envir = .GlobalEnv)

# Table qui va contenir les r?sultats du diagnostic
diagnostic <- choice %>% mutate(diagnostic=rep(NA, nrow(choice)))
assign("diagnostic", diagnostic, envir = .GlobalEnv)


# Estimation des paramètres pour chaque couple manip / produit
for (ch in 1:nrow(choice)){
  # Données restreintes
  data.ch <- data %>% filter(essay == choice$essay[ch], EO == choice$EO[ch])
  data.ch <- data.ch %>% mutate(essay = factor(essay), EO = factor(EO))
  tryCatch({
    if (bas0==TRUE){
    # Initialisation des paramètres
    init <- c(max(data.ch$OD), 0, 1)
    assign("init", init, envir = .GlobalEnv)
    
    par <- c("top",  "logIC50", "slope")
    assign("par", par, envir = .GlobalEnv)
    
    names(init) <- par
    assign("init", init, envir = .GlobalEnv)
    
    # Modèle 
    model <- nlse_lite(data=data.ch,
                       f="logistic4_bottom0", 
                       init=init,
                       x='conc',
                       y='OD',
                       groups=T,
                       groups.col = c('EO','essay'),
                       groups.par = list(par, par),
                       call=T,
                       result=F,
                       maxiter = 50)
    assign("model", model, envir = .GlobalEnv)
    
    } else{
      # Initialisation des paramètres
      init <- c(max(data.ch$OD), min(data.ch$OD), 0, 1)
      assign("init", init, envir = .GlobalEnv)
      
      par <- c("top", "bottom", "logIC50", "slope")
      assign("par", par, envir = .GlobalEnv)
      
      names(init) <- par
      assign("init", init, envir = .GlobalEnv)
      
      # Modèle 
      model <- nlse_lite(data=data.ch,
                         f="logistic4", 
                         init=init,
                         x='conc',
                         y='OD',
                         groups=T,
                         groups.col = c('EO','essay'),
                         groups.par = list(par, par),
                         call=T,
                         result=F,
                         maxiter = 50)
      assign("model", model, envir = .GlobalEnv)
      
      }
    # Pas pour la grille de logCI50 possibles
    a <- floor(log10(min(data.ch$conc[data.ch$conc>0])))
    assign("a", a, envir = .GlobalEnv)
    
    b <- ceiling(log10(max(data.ch$conc)))
    assign("b", b, envir = .GlobalEnv)
    
    pas <- seq(a, b, length.out = 11)[2] - seq(a, b, length.out = 11)[1]
    assign("pas", pas, envir = .GlobalEnv)
    
    # Optimisation des valeurs initales pour les logCI50
    init.new <- modif_init(model$args$start, data.ch, pas = pas)
    assign("init.new", init.new, envir = .GlobalEnv)
    
    model$args$start <- init.new
    assign("model", model, envir = .GlobalEnv)
    
    # Estimation
    do.call(model$optimiser,model$args)
    # Diagnostic
    diagnostic$diagnostic[ch] <- "Aucun problème d'estimation"
    assign("diagnostic", diagnostic, envir = .GlobalEnv)
    
  },
  error = function(e){
    # Diagnostic
    diagnostic$diagnostic[ch] <- "Problèmes d'estimation"
    assign("diagnostic", diagnostic, envir = .GlobalEnv)
    return(NULL)
  })
}

# Sauvegarde du diagnistic dans un fichier .csv
#write.csv2(diagnostic, file=paste(output, "diagnostic.csv", sep="/"), row.names=FALSE)
return(diagnostic)
}




