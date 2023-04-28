###############################################################################################
#
# Programme permettant de calculer des CI50 pour plusieurs manips / composés et de comparer ces 
# composés entre eux de manière globale et par paires
#
###############################################################################################


# VIDER L'ENVIRONNEMENT 
rm(list = ls())


# FERMETURE FICHIERS OUVERTS (AU CAS OU)
tryCatch({
  dev.off()
},error = function(e){
  return(cat("\n"))
})

# LISTE DES PACKAGES DEVANT ETRE INSTALLES
# "Rcpp"
# "ggplot2"
# "scales"
# "ggthemes"
# "RColorBrewer"
# "boot"
# "minpack.lm"
# "doParallel"
# "dplyr"
# "nlstools"

# DONNEES A ENTRER PAR L'UTILISATEUR (#### ??? ####)
# répertoire de travail
directory <- "C:/Users/Administrateur/Desktop/2223_M1/Projet M1/ProgrammeR"          #### ??? ####
# nom du fichier contenant les donn?es 
file <- "AuroreK Vi liquide 552 1 manip_0422.csv"                   #### ??? ####
# noms des colonnes utiles dans le fichier 
conc <- "conc_mg_L" # concentrations                              #### ??? ####
OD <- "DO"          # densit?s optiques                           #### ??? ####
EO <- "HE"          # compos?s                                    #### ??? ####
essay <- "manip"    # manipulations                               #### ??? ####
# labels pour les graphiques
#conc.unit <- "mg/L"                                              #### ??? ####
conc.label <- "concentration (mg/L)"                              #### ??? ####
conc.unit <- "mg/L"
OD.label <- "optical density"                                     #### ??? ####
# paramètre graphique à augmenter quand le haut est mal représenté
opti.graph <- 100                                                 #### ??? ####
# nombre de composés
nb.EO <- 7                                                        #### ??? ####
# nombre de manips
nb.essay <- 1                                                     #### ??? ####
# nom du dossier qui va contenir les résultats de l'analyse
main.output <- "EncoreUnExemple"                                      #### ??? ####
# Choix méthodologies
# proportion pour la concentration inhibitrice
prop <- 50                                                        #### ??? ####
# CI50 partagée entre manip (TRUE ou FALSE)
# Sauf cas très exceptionnel, utiliser same.IC50 <- FALSE
same.IC50 <- FALSE                                                 #### ??? ####
# Paramètre bas fixé à 0 (TRUE ou FALSE)
# Il faut utiliser bas0 <- FALSE, sauf cas exceptionnels
bas0 <- FALSE                                                    ### ??? ####
# Etapes à réaliser (TRUE ou FALSE)
statistics.call <- TRUE        # statistiques descriptives        #### ??? ####
estimation.call <- TRUE        # calcul des CI50                  #### ??? ####
test.global.call <- TRUE      # comparaison globale des CI50     #### ??? ####

# AUTRES DONNEES : NE PAS MODIFIER !! (POUR INFORMATION)
# niveau intervalles de confiance
lvl <- 0.95
# seuil convergence
conv.ci.lvl <- 0.7
# risque alpha pour les tests
risk <- 0.05
# méthode pour calculer les IC (intervalle de confiance) : "bootstrap" ou "Wald"
ci.method <- "bootstrap"                                          
# nombre d'échantillons bootstrap utilisés
B.boot <- 10
# nombre de permutations
B.perm <- 1000
# comparaisons globale :  "test F"  ou "test permutation"
test.global <- "test F" 
# comparaisons mutiples : "IC" (ci.method <- "bootstrap") ou "test F"  ou "test permutation"
test.pairs <- "IC"
# nombre maximum d'itérations pour l'algo. de Levenberg-Marquardt
maxiter <- 50
# taille de la grille pour l'optimisation des logCI50 initiaux
taille.grille <- 11


# VERIFICATION EXISTENCE REPERTOIRE DE TRAVAIL
if (!dir.exists(directory)){
  stop("le répertoire de travail fourni par l'utilisateur n'existe pas")
} 



# MISE A JOUR DU REPERTOIRE DE TRAVAIL
setwd(directory)



# APPEL DU PROGRAMME
source("programme.R")





