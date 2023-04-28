statistic <- function(data){
  output <- paste(main.output, "statistiques_descriptives", sep="/")
  # graphique
  # sauvegarde dans un fichier .png
  #png(file = paste(output, "scatter_plots.png", sep="/"), width = 500)
  # graphique
  
  # moyennes, ?cart-types et nombre de r?p?titions par par compos?, par manip et par concentration
  stat <- data %>% group_by(EO, essay, conc) %>%
    summarise(mean = mean(OD), sd = sd(OD), repetitions = n())
  
  write.csv2(stat, file=paste(output, "statistics.csv", sep="/"), row.names=F)
  return(stat)
}  
