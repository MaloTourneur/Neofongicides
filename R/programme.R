# PACKAGES 
tryCatch({ # Gestion des erreurs
  # Si pas erreur : chargement des packages
  source("packages.R")
},# Erreur : affichage message d'information
error = function(e){
  return(cat("Erreur : un des packages nécessaires n'est pas installé"))
})



# FONCTIONS R
source("Fonctions/modif_init.R")
source("Fonctions/nlse_lite.R")
source("Fonctions/test_F.R")
source("Fonctions/test_permutation.R")
source("Fonctions/ic_bootstrap.R")
source("Fonctions/LetterDisplay.R")

# IMPORTATION DES DONNEES
source("importation_donnees.R")

# PREPARATION DES DONNEES
source("preparation_donnees.R")

# DOSSIERS QUI VONT CONTENIR LES RESULTATS :
# si le dossier n'existe pas : cr?ation du dossier
if (!dir.exists(main.output)){ 
  dir.create(main.output)
}



# Dossier pour l'analyse descriptive (si elle est demandée)
if (statistics.call){
  if (!dir.exists(paste(main.output, "statistiques_descriptives", sep="/"))){
    dir.create(paste(main.output, "statistiques_descriptives", sep="/"))
  }
}
# Dossiers pour la régression, les tests et un résumé des résultats si la régression 
# a été demandé
if (estimation.call){
  # Régression
  if (!dir.exists(paste(main.output, "calcul_CI50", sep="/"))){
    dir.create(paste(main.output, "calcul_CI50", sep="/"))
  }
  # Tests
  if (!dir.exists(paste(main.output, "comparaison_CI50", sep="/"))){
    dir.create(paste(main.output, "comparaison_CI50", sep="/"))
  }
  # Résumé
  if (!dir.exists(paste(main.output, "resume", sep="/"))){
    dir.create(paste(main.output, "resume", sep="/"))
  }
}

# STATISTIQUES DESCRIPTIVES
# (si elle a été demandée)
if (statistics.call){
  # Dossier pour enregistrer les résultats
  output <- paste(main.output, "statistiques_descriptives", sep="/")
  # Appel code
  source("statistiques.R")
}

# CHOIX CIp (p=50, 90, ...)
# Définition de la fonction logistique
if (bas0 == TRUE){
  source("Fonctions/logistic4_bottom0.R")
} else{
  source("Fonctions/logistic4.R")
}
  
IC50.label <- paste("IC", prop, sep="")
logIC50.label <- paste("logIC", prop, sep="")

# CALCUL DES CI50
# (si il a été demandé)
estimation.done <- FALSE
if (estimation.call){
  # Dossier pour enregistrer les résultats
  output <- paste(main.output, "calcul_CI50", sep="/")
  # CI50 partagées entre manip
  if (same.IC50){
    tryCatch({ # Gestion erreur d'estimation
      source("calcul_CI50_commune.R")
      source("sorties_calcul_CI50_commune.R")
      estimation.done <- TRUE
    },
    error = function(e){
      return(cat("Erreur : calcul de CI50 partagées entre manip impossible\n"))
    })
  # CI50 par manip
  }else{
    # plusieurs essais
    if (nb.essay > 1){ 
      tryCatch({ # Gestion erreur d'estimation
        source("calcul_CI50_diff.R")
        source("sorties_calcul_CI50_diff.R")
        estimation.done <- TRUE
      },
      error = function(e){
        return(cat("Erreur : calcul de CI50 par manip impossible\n"))
      })
    # un seul essai
    }else{            
      tryCatch({ # Gestion erreur d'estimation
        source("calcul_CI50_commune.R")
        source("sorties_calcul_CI50_commune.R")
        estimation.done <- TRUE
      },
      error = function(e){
        return(cat("Erreur : calcul de CI50 impossible pour la manip\n"))
      })
    }
  }
}

# DIAGNOSTIC ESTIMATION
# (si le modèle demandé n'a pas pu être ajusté)
if (!estimation.done){
  source("diagnostic_calcul_CI50.R")
}

# COMPARAISON DES CI50
# (si des CI50 ont pu ?tre calcul?es pour plusieurs produits)
if ((nb.EO > 1)&(estimation.call)&(estimation.done)){
  output <- paste(main.output, "comparaison_CI50", sep="/")
  if (same.IC50){
    source("comparaison_CI50_commune.R")
    source("sorties_comparaison_CI50_commune.R")
  }else{
    if (nb.essay > 1){
      source("comparaison_CI50_diff.R")
      source("sorties_comparaison_CI50_diff.R")
    }else{
      source("comparaison_CI50_commune.R")
      source("sorties_comparaison_CI50_commune.R")
    }
  }
}

# RESUME
# Le programme suivant n?cessite une variable test.pairs.done
if (nb.EO == 1){
  test.pairs.done <- FALSE
}

# (si des CI50 ont pu être calculées)
if ((estimation.call)&(estimation.done)){
  output <- paste(main.output, "resume", sep="/")
  if (same.IC50){
    source("resume_CI50_commune.R")
  }else{
    if (nb.essay > 1){
      source("resume_CI50_diff.R")
    }else{
      source("resume_CI50_commune.R")
    }
  }
}



# FERMETURE FICHIERS OUVERTS (AU CAS OU)
tryCatch({
  dev.off()
},error = function(e){
  return(cat("\n"))
})




