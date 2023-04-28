preparation_data <- function(data, conc, OD, EO, essay, nb.EO, nb.essay){
  
  # v?rification de l'existence des noms de colonnes
  if (!all(c(conc, OD, EO, essay) %in% colnames(data))){
    stop("au moins l'un des noms de colonnes est faux ou n'existe pas")
  }
  
  # supression des colonnes inutiles dans la table
  data <- data[, c(conc, OD, EO, essay)]
  
  # modification du nom des colonnes de la table
  colnames(data)[colnames(data) %in% c(conc, OD, EO, essay)] <- c("conc", "OD", "EO", "essay")
  
  # supression des lignes contenant des valeurs manquantes
  nb.obs <- nrow(data)
  data <- data[apply(data, 1, function(x) all(!is.na(x))),]
  nb.obs.delete <- nb.obs - nrow(data)
  if (nb.obs.delete > 0){
    cat(paste("Info : ",
              toString(nb.obs.delete),
              " lignes ont été supprimées car il y a des données manquantes\n",
              sep="")
    )
  }
  
  # v?rification : existence de donn?es dans la table (2)
  if (nrow(data) == 0){
    stop("la table ne contient pas de données")
  }
  
  # facteurs pour les compos?s et les essais
  data$EO <- as.character(data$EO)
  data$EO <- as.factor(data$EO)
  data$essay <- as.character(data$essay)
  data$essay <- as.factor(data$essay)
  
  
  data$conc <- gsub(",", ".", data$conc)
  data$conc <- as.numeric(data$conc)
  
  data$OD <- gsub(",", ".", data$OD)
  data$OD <- as.numeric(data$OD)
  
  # # v?rification : nombre de facteurs
  # if(length(levels(data$EO)) != nb.EO){
  #   stop("le nombre de composés indiqué par l'utilisateur ne correspond pas
  #        au nombre de composés présents dans la table")
  # }
  # if(length(levels(data$essay)) != nb.essay){
  #   stop("le nombre de manipulations indiqué par l'utilisateur ne correspond pas
  #        au nombre de manipulations présentes dans la table")
  # }
  
  nb.EO = length(levels(data$EO))
  nb.essay = length(levels(data$essay))
  
  # v?rification : pas de facteurs communs pour les compos?s et les manipulations
  if (any(levels(data$EO) %in% levels(data$essay))){
    stop("un même code est utilisé pour un composé et une manipulation")
  }
  
  # remplacement des ?ventuels espaces dans les facteurs pour les compos?s / manips
  levels(data$EO) <- gsub(" ", levels(data$EO), replacement = "_")
  levels(data$essay) <- gsub(" ", levels(data$essay), replacement = "_")
  
  data <- na.omit(data)
  
  return(data)
}