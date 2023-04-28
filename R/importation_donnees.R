# v?rification existence fichier
if (!file.exists(file)){
  stop("le fichier de données fourni par l'utilisateur n'existe pas")
}

# v?rification : fichier vide
if (file.size(file) == 0){
  stop("le fichier de données fourni par l'utilisateur est vide")
}

# cr?ation de la table
data <- read.csv2(file)

# v?rification : existence de donn?es dans la table (1)
if (nrow(data) == 0){
  stop("la table de données ne contient aucune observation")
}