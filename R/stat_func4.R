statistic4 <- function(data){
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
      plt3 <- ggplot(stat, aes(x=conc, y=mean, fill=conc)) + geom_col() + 
        ggtitle(paste("Means and standard deviations for \n optical densities for essay", e, sep=" ")) +
        geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
        facet_wrap(~EO, scales = "free_x") +
        theme_hc() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_fill_manual(values = ma_palette) + 
        xlab(conc.label) + ylab(OD.label) + guides(fill=FALSE)
      print(plt3)
      return(plt3)
    }
  }
}
