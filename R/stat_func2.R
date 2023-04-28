statistic2 <- function(data){
  
  output <- paste(main.output, "statistiques_descriptives", sep="/")
  
  # scatterplots : densit?s optiques en fonction des concentrations (par manip et par compos?)
  # graduations pour l'axe x
  a <- floor(log10(min(data$conc[data$conc>0])))
  b <- ceiling(log10(max(data$conc))) 
  breaks <- c(0, 10^(a:b))
  # graphique
  # sauvegarde dans un fichier .png
  png(file = paste(output, "scatter_plots.png", sep="/"), width = 500)
  # graphique
  # donn?es
  plt1 <- ggplot(data) + 
    # points
    # aes : nom des colonnes dans data
    # alpha : param?tre de transparence
    # col : facteur pour la couleur, ici couleur diff?rente pour chaque essai
    geom_point(aes(x=conc, y=OD, col=essay), alpha=0.8) +
    # titre du graphique
    ggtitle("Plot of optical densities against \n concentrations for each product") +
    # ?chelle pseudo-logarithmique pour l'axe x
    # sigma : position du z?ro
    # base : 10 pour log10
    # breaks : ?chelle
    scale_x_continuous(trans=pseudo_log_trans(sigma=(10^a)/10, base=10), breaks = breaks) + 
    # un graphique pour chaque EO
    facet_wrap(~EO) + 
    # label axe y
    ylab(OD.label) + 
    # label axe x
    xlab(conc.label) +
    # label couleurs
    labs(col="essay") + 
    # theme
    theme_hc() +
    # palette de couleurs
    scale_colour_brewer(palette="Dark2") +
    # rotation de l'?chelle pour l'axe x de 45?
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    # centrer le titre du graphique
    theme(plot.title = element_text(hjust = 0.5)) +
    # l?gende ? droite du graphique
    theme(legend.position="right") 
  # sortie graphique
  print(plt1)
  # fermeture fichier .png
  dev.off()
  return(plt1)
  
}

statistic3 <- function(data){
  
  output <- paste(main.output, "statistiques_descriptives", sep="/")
  
  # scatterplots : densit?s optiques en fonction des concentrations (par manip et par compos?)
  # graduations pour l'axe x
  a <- floor(log10(min(data$conc[data$conc>0])))
  b <- ceiling(log10(max(data$conc))) 
  breaks <- c(0, 10^(a:b))
  
  if ((nb.essay > 1)|((nb.essay == 1)&(nb.EO == 1))){
    # barplot
    # pour chaque compos?
    for (comp in levels(data$EO)){
      # moyennes et ?carts-types des densit?s optiques par concentration et par manip
      stat <- data %>% group_by(EO, essay, conc) %>% summarise(mean = mean(OD), sd = sd(OD)) %>%
        filter(EO==comp)
      stat$conc <- as.factor(stat$conc)
      # palette de couleurs pour le graphique
      nb.conc <- length(levels(stat$conc))
      ma_palette <- brewer.pal(9, name = "Blues")[3:9]
      taille.palette <- length(ma_palette)
      ma_palette <- c(ma_palette, rep(tail(ma_palette, 1), nb.conc - taille.palette))
      # graphique
      png(file = paste(output, "/barplot_", comp, ".png", sep=""), width = 500)
      plt2 <- ggplot(stat, aes(x=conc, y=mean, fill=conc)) + geom_col() + 
        ggtitle(paste("Means and standard deviations for \n optical densities for product", comp, sep=" ")) +
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
        facet_wrap(~essay, scale="free_x") +
        theme_hc() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = ma_palette) + 
        xlab(conc.label) + ylab(OD.label) + guides(fill=FALSE)
      print(plt2)
      dev.off()
      return(plt2)
    }
  }
  
}



statistic4 <- function(data){
  
  output <- paste(main.output, "statistiques_descriptives", sep="/")
  
  # scatterplots : densit?s optiques en fonction des concentrations (par manip et par compos?)
  # graduations pour l'axe x
  a <- floor(log10(min(data$conc[data$conc>0])))
  b <- ceiling(log10(max(data$conc))) 
  breaks <- c(0, 10^(a:b))
  
  # Si il y a plusieurs produits :
  if (nb.EO > 1){
    # barplot
    # pour chaque essai
    for (e in levels(data$essay)){
      # moyennes et ?carts-types des densit?s optiques par concentration et par manip
      stat <- data %>% group_by(EO, essay, conc) %>% summarise(mean = mean(OD), sd = sd(OD)) %>%
        filter(essay==e)
      stat$conc <- as.factor(stat$conc)
      # palette de couleurs pour le graphique
      nb.conc <- length(levels(stat$conc))
      ma_palette <- brewer.pal(9, name = "Blues")[3:9]
      taille.palette <- length(ma_palette)
      ma_palette <- c(ma_palette, rep(tail(ma_palette, 1), nb.conc - taille.palette))
      # graphique
      png(file = paste(output, "/barplot_", e, ".png", sep=""), width = 500)
      plt3 <- ggplot(stat, aes(x=conc, y=mean, fill=conc)) + geom_col() + 
        ggtitle(paste("Means and standard deviations for \n optical densities for essay", e, sep=" ")) +
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
        facet_wrap(~EO, scales = "free_x") +
        theme_hc() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = ma_palette) + 
        xlab(conc.label) + ylab(OD.label) + guides(fill=FALSE)
      print(plt3)
      dev.off()
      return(plt3)
    }
  }
}
