# Loading and attaching packages
library(minpack.lm)

# Packages
library(boot)

# Fonction qui calcule des intervalles de confiance avec des m?thodes de bootstrap (quantiles / BCa)
# ARGUMENTS :
# - model : le mod?le ajust? pour lequel on veut calculer des intervalles de confiance
#           (objet contenant la fonction utlis?e et les arguments)
# - par : les param?tres du mod?le pour lesquelles on veut des IC
# - B : le nombre d'?chantillons bootstrap utilis?s
# - level : le niveau des intervalles de confiance
# RETOURNE : les intervalles de confiance calcul?s  (ic) ainsi que le nombre d'?chantillons 
#            bootstrap pour lesquels il y a eu des probl?mes d'estimation (conv) et la 
#            distribution bootstrap obtenue

ic_bootstrap <- function(model,par,B=1000,level=0.95){
  # Fonction pour estimer les parametres
  estimation <- function(data,index,model){
    tryCatch(
      {
        # Re-?chantillonage
        data.b <<- data[index,]
        # Modification des arguments (nouvelles donn?es)
        args.b <- model$args
        args.b$data <- data.b
        # Ajustement du mod?le sur donn?es re-?chantillonn?es
        model.b <- do.call(model$optimiser,args.b)
        # Param?tres estim?s pour l'?chantillon bootstrap retourn?s
        return(summary(model.b)$coefficients[par,1]) 
      },
      # Si probl?me de convergence : on retourne des NA
      error = function(e){
        return(NA)
      }
    )
  }
  # Echantillons bootstrap
  bootObject <- boot(data=model$args$data,statistic=estimation,R=B,model=model)
  conv <- B - length(which(is.na(bootObject$t[,1])))
  # Supression des NA (probl?me gradient)
  bootObject$t <- bootObject$t[!is.na(bootObject$t[,1]),]
  # Calcul de la taille de l'?chantillon bootstrap sans les NA
  bootObject$R <- conv
  # Calcul de l'IC
  # IC pour chaque param?tres stock?s dans une liste
  ci <- list()
  for (i in 1:length(par)){ # un ou plusieurs param?res ?
    if (length(par) == 1){
      ci[[i]] <- boot.ci(bootObject, conf = level, type = "perc",t=bootObject$t,t0=bootObject$t0)
    }else{
      ci[[i]] <- boot.ci(bootObject, conf = level, type = "perc",index=i)
    }
    ci[[i]] <- ci[[i]]$percent[4:5]
  }
  # Conversion en matrice dont les lignes sont les IC des diff?rents param?tres
  # M?thode des quantiles
  ci <- matrix(unlist(ci),ncol=2,byrow=T)
  rownames(ci) <- par
  # On retourne les intervalles de confiance et le nombre d'?chantillons bootstrap pour lesquels
  # l'ajustement d'un mod?le n'a pas ?t? possible
  return(list(ci=ci,conv=conv,distr=bootObject$t))
}


# Function for nonlinear regression (with Gauss-Newton and Levenberg-Marquardt algorithms)
# with less options than nlse function
# Arguments :
# - data : a dataset
# - f : a regression function as a string
# - init : initial values for regression parameters
# - x : a string containing the name of the explicative variable
# - y : a string containing the name of the explained variable
# - groups : a logical value indicating if we want to do regression with grouped data
# - groups.col : a vector containing groups variables as strings
# - groups.par : a list of vectors, each vector contain the parameters that will depend on the group variable
# - call : a logical value indicating whether the function should return call information
# - result : a logical value indicating whether the function should return regression output
# - tol : crit?re d'arr?t (tol?rance)
# - maxiter : crit?re d'arr?t (nombre max d'it?rations)
# Retourne : le mod?le ajust? (objet de type nls) si result=TRUE et/ou la fonction et les
#            arguments utlis?s dans la fonction permettant d'ajuster le mod?le non lin?aire

nlse_lite <- function(data,f,init,x,y,
                      groups=FALSE,groups.col=NULL,groups.par=NULL,
                      call=FALSE,result=TRUE,tol=sqrt(.Machine$double.eps),
                      maxiter=50){
  #optimizer
  optimiser="nlsLM"
  # control arguments
  ctrl <- nls.lm.control(ftol=tol,maxiter = maxiter)
  # no grouped data
  if (groups==FALSE){ 
    # regression parameters as a string : par1,par2,par3,...
    par <- paste(names(init), collapse = ",")
    # formula for optimiser : a string containing formula and converted to formula type
    formula <- formula(paste(y," ~ ",f,"(",x,",",par,")",sep=""))
    # arguments for optimiser as a list
    args <- list(formula,data=data,start=init,
                 control=ctrl,trace=FALSE)
    # grouped data
  }else{
    # formula for optimiser (a string)
    formula <- paste(y," ~ ", sep="")
    # vector for initial values for grouped regression parameters
    init.groups <- c()
    #  a list for levels for each group factor
    groups.list <- list(length=length(groups.col))
    for (i in 1:length(groups.col)){
      # group factor
      group.col <- data[,groups.col[i]]
      # levels
      groups.list[[i]] <- levels(group.col)
    }
    # data frame with all possible combinations for group factors values
    choices <- expand.grid(groups.list)
    colnames(choices) <- groups.col
    # construction of regression formula / vector of initial values for regression parameters 
    # for each combination for group factors values
    for (ch in 1:nrow(choices)){
      par <- names(init)
      for (i in 1:length(groups.col)){
        formula <- paste(formula,"(",groups.col[i],"==\"",choices[ch,i],"\")","*",sep="")
        for (j in 1:length(names(init))){
          if (names(init)[j] %in% groups.par[[i]]){
            par[j] <- paste(par[j],".",choices[ch,i],sep="")
          }
        }
      }
      init.groups <- c(init.groups,paste(par,"=",init,sep=""))
      par <- paste(par, collapse = ",")
      formula <- paste(formula,f,"(",x,",",par,")",sep="")
      formula <- paste(formula,"+",sep="")
    }
    formula <- substr(formula, 1, nchar(formula)-1)
    formula <- formula(formula)
    init.groups <- paste(unique(init.groups),collapse=",")
    init.groups <- paste("c(",init.groups,")",sep="")
    init.groups <- eval(parse(text=init.groups))
    args <- list(formula,data=data,start=init.groups,
                 control=ctrl,trace=FALSE)
  }
  # list for function outputs
  function.return <- list()
  # regression results 
  if (result==TRUE) 
    function.return <- c(function.return,list(result=do.call(optimiser,args)))
  # optimiser and optimiser's arguments
  if (call==TRUE){
    function.return <- c(function.return,list(optimiser=optimiser,args=args))
  }
  return(function.return)
}

# Fonction pour modifier l'initialisation pour le paramètre logCIp
# (logCIp différentes pour les huiles essentielles)
# Arguments : 
# - init : paramètres initiaux avant modification
# - data : données sur lesquelles on va effectuer la régression
# - nbiter : nombre d'itérations à effectuer pour choisir les paramètres initiaux
# - pas : pas pour la grille des valeurs à essayer pour le paramètre logCIp 
# - w : poids pour les moindres carrés pondérés
# Retourne : les paramètres initiaux modifiés en fonction des données

modif_init <- function(init,data,nbiter=1,pas=1,bas0=FALSE){
  # stocker les logCIp initiales dans un autre vecteur 
  init.modif <- init[grepl("logIC50",names(init))]
  # grille de logCIp ? essayer
  grid.logCIp <- seq(floor(log10(min(data$conc[data$conc>0]))),ceiling(log10(max(data$conc))),pas)
  # modèle pour chaque huile essentielle
  
  if (bas0 == TRUE){
    model <- nlse_lite(f='logistic4_bottom0',
                       init=c(top=max(data$OD),logIC50=1,slope=1),
                       data=data,x='conc',y='OD',call=T,result=F,maxiter=nbiter+1)    
  } else{
    model <- nlse_lite(f='logistic4',
                       init=c(top=max(data$OD),bottom=min(data$OD),logIC50=1,slope=1),
                       data=data,x='conc',y='OD',call=T,result=F,maxiter=nbiter+1)    
  }
  # pour chaque huile essentielle eo : calcul de la valeur optimale pour la logCIp intiale
  for (eo in levels(data$EO)){
    # données pour eo
    data.eo <- data[data$EO==eo,]
    data.eo$EO <- factor(data.eo$EO)
    # modification des données dans le modèle
    model$args$data <- data.eo
    # vecteur pour stocker les sommes des résidus au carré
    RSS <- vector(length = length(grid.logCIp))
    for (l in 1:length(grid.logCIp)){
      # modification de la logCIp initiale dans le modèle
      model$args$start['logIC50'] <- grid.logCIp[l]
      # ajustement du modèle et stockage de la somme des résidus au carré
      RSS[l] <- tryCatch(# pour gérer les problèmes de convergence
        {
          model.l <- summary(do.call("nlsLM",model$args))
          # pour vérifier la cohérence des paramètres estimés
          if (bas0==FALSE){
            if ((model.l$coefficients['slope',1]>0)&(model.l$coefficients['top',1]>model.l$coefficients['bottom',1])){
              sum(model.l$residual^2) 
            }else{
              NA
            }
          } else{
            if ((model.l$coefficients['slope',1]>0)&(model.l$coefficients['top',1]>0)){
              sum(model.l$residual^2)
            }else{
              NA
            }              
          }
        },
        error = function(e){
          return(NA)
        })
    }
    # choix de la valeur initiale la plus prometteuse
    new.value <- grid.logCIp[which(RSS==min(RSS,na.rm = T))[1]]
    # modifier la valeur de la logCIp initiale pour l'huile essentielle eo
    init.modif[matrix(unlist(strsplit(names(init.modif),"[.]")),nrow=2)[2,]==eo] <- new.value
  }
  # modifier les logCIp initiales dans le vecteur des paramètres initiaux
  init[grepl("logIC50",names(init))] <- init.modif
  # retourner le vecteur ainsi modifi?
  return(init)
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








calcul_CI50_commune <- function(data, bas0, taille.grille, nb.essay, B.boot, lvl, conv.ci.lvl){
# Initialisation des paramètres  
if (bas0 == TRUE){
init <- c(max(data$OD),  0, 1)
assign("init", init, envir = .GlobalEnv)

par <- c("top",  "logIC50", "slope")
assign("par", par, envir = .GlobalEnv)

names(init) <- par
assign("names(init)", names(init), envir = .GlobalEnv)

} else{
  init <- c(max(data$OD), min(data$OD), 0, 1)
  assign("init", init, envir = .GlobalEnv)
  
  par <- c("top", "bottom", "logIC50", "slope")
  assign("par", par, envir = .GlobalEnv)
  
  names(init) <- par  
  assign("names(init)", names(init), envir = .GlobalEnv)
  
}

# Modèle 
if (bas0 == TRUE){
  model <- nlse_lite(data=data,
                   f="logistic4_bottom0", 
                   init=init,
                   x='conc',
                   y='OD',
                   groups=T,
                   groups.col = c('EO','essay'),
                   groups.par = list(par, par[c(1,3)]),
                   call=T,
                   result=F,
                   maxiter = maxiter)
  assign("model", model, envir = .GlobalEnv)
  
} else{
  model <- nlse_lite(data=data,
                     f="logistic4", 
                     init=init,
                     x='conc',
                     y='OD',
                     groups=T,
                     groups.col = c('EO','essay'),
                     groups.par = list(par, par[c(1,2,4)]),
                     call=T,
                     result=F,
                     maxiter = maxiter) 
  assign("model", model, envir = .GlobalEnv)
  
}
  
  
# Pas pour la grille de logCI50 possibles
a <- floor(log10(min(data$conc[data$conc>0])))
assign("a", a, envir = .GlobalEnv)
b <- ceiling(log10(max(data$conc)))
assign("b", b, envir = .GlobalEnv)

pas <- seq(a, b, length.out = taille.grille)[2] - seq(a, b, length.out = taille.grille)[1]
# Optimisation des valeurs initales pour les logCI50
init.new <- modif_init(model$args$start, data, pas = pas, bas0=bas0)
model$args$start <- init.new

# Estimation
estimation <- do.call(model$optimiser,model$args)

# Sauvegarde des résidus
residuals <- estimation$m$resid()
assign("residuals", residuals, envir = .GlobalEnv)

# Paramètres pour tous les essais
estimation <- summary(estimation)$coefficients[,1]
for (comp in levels(data$EO)){
  estimation[paste("logIC50", comp, levels(data$essay), sep=".")] <- rep(estimation[paste("logIC50", comp, sep=".")], nb.essay)
  estimation <- estimation[names(estimation) != paste("logIC50", comp, sep=".")]
  assign("estimation", estimation, envir = .GlobalEnv)
  
}

# Intervalles de confiance
# Bootstrap
if (ci.method == "bootstrap"){
  ci <- ic_bootstrap(model, names(model$args$start), B=B.boot, level=lvl)
  conv.ci <- ci$conv
  assign("conv.ci", conv.ci, envir = .GlobalEnv)
  
  distr.ci <- ci$distr
  assign("distr.ci", distr.ci, envir = .GlobalEnv)
  
  colnames(distr.ci) <- names(model$args$start)
  assign("distr.ci", distr.ci, envir = .GlobalEnv)
  
  ci <- ci$ci
  assign("ci", ci, envir = .GlobalEnv)
# Wald
}else if(ci.method == "Wald"){
  ci <- confint2(do.call(model$optimiser,model$args))
  assign("ci", ci, envir = .GlobalEnv)
}
# IC pour tous les essais
for (comp in levels(data$EO)){
  ci <- rbind(matrix(rep(ci[paste("logIC50", comp, sep="."),], nb.essay), ncol=2, byrow=T), ci)
  rownames(ci)[1:nb.essay] <- paste("logIC50", comp, levels(data$essay), sep=".")
  ci <- ci[rownames(ci) != paste("logIC50", comp, sep="."),]
  assign("ci", ci, envir = .GlobalEnv)
  
}
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

