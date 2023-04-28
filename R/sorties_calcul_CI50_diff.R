plot1_calcul_CI50_diff <- function(){
  output <- paste(main.output, "calcul_CI50", sep="/")
# Graphiques : r?sidus
data.residuals <- data.frame(conc=data.essay$conc, residuals)
assign("data.residuals", data.residuals, envir = .GlobalEnv)

# Scatterplot
breaks <- c(0, 10^(a:b))
assign("breaks", breaks, envir = .GlobalEnv)

png(file = paste(output, "residuals_scatterplot.png", sep="/"), width = 500)
plt <- ggplot(data.residuals) + geom_point(aes(x=conc, y=residuals), alpha=0.5) +
  ggtitle("Scatterplot of residuals") +
  scale_x_continuous(trans=pseudo_log_trans(sigma=(10^a)/10, base=10), breaks = breaks) +
  geom_hline(yintercept = 0, col='red', size = 1) +
  ylab("residuals") + xlab(conc.label) +
  theme_hc() + scale_colour_brewer(palette="Dark2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
print(plt)
dev.off()
return(plt)

}

plot2_calcul_CI50_diff <- function(){
  output <- paste(main.output, "calcul_CI50", sep="/")
# Histogramme
png(file = paste(output, "residuals_histogram.png", sep="/"), width = 500)
plt <- ggplot(data.residuals) +
  ggtitle("Histogram of residuals") +
  geom_histogram(aes(x=residuals, y=..density..), alpha=0.4, col="grey", fill="blue") +
  stat_function(fun = dnorm,
                args = list(mean = mean(data.residuals$residuals), sd = sd(data.residuals$residuals)),
                color="red", size=1) + 
  theme_hc() + scale_colour_brewer(palette="Dark2") +
  theme(plot.title = element_text(hjust = 0.5))
print(plt)
dev.off()
return(plt)
}

plot3_calcul_CI50_diff <- function(){
  output <- paste(main.output, "calcul_CI50", sep="/")
# QQ-plot
png(file = paste(output, "residuals_QQplot.png", sep="/"), width = 500)
plt <- ggplot(data.residuals, aes(sample=residuals)) + stat_qq(alpha=0.5) + 
  ggtitle("QQ-plot of residuals") +
  stat_qq_line(col="red", size=1) + theme_hc() + scale_colour_brewer(palette="Dark2") +
  xlab("normal quantiles") + ylab("residuals") +
  theme(plot.title = element_text(hjust = 0.5))
print(plt)
dev.off()
return(plt)

}

plot4_calcul_CI50_diff <- function(){
  output <- paste(main.output, "calcul_CI50", sep="/")
plot_list <- list()
breaks <- c(0, 10^(a:b))
# Graphiques : courbes estim?es pour chaque produit
for (comp in levels(data$EO)){
  # donn?es pour le produit consid?r?
  data.comp <- data %>% filter(EO == comp)  %>% select(-EO)
  data.comp <- do.call("rbind", replicate(50, data.comp, simplify = FALSE))
  data.comp$conc2 <- data.comp$conc
  for (e in levels(data.comp$essay)){
    cc <- filter(data.comp, essay == e)$conc
    data.comp[data.comp$essay == e, "conc2"] <- 
      c(0, 10^seq(floor(log10(min(cc[cc>0])/opti.graph)), log10(max(cc)), length.out = length(cc) - 1))
  }
  if (bas0==TRUE){
  data.comp <- data.comp %>% 
    mutate(curve=logistic4_bottom0(conc2, 
                           estimation[paste(par[1], comp, essay, sep=".")],
                           estimation[paste(par[2], comp, essay, sep=".")],
                           estimation[paste(par[3], comp, essay, sep=".")]),
           IC50=10^estimation[paste(par[2], comp, essay, sep=".")],
           IC50.inf=10^ci[paste(par[2], comp, essay,  sep="."), 1],
           IC50.sup=10^ci[paste(par[2], comp, essay,  sep="."), 2])
  } else{
    data.comp <- data.comp %>% 
      mutate(curve=logistic4(conc2, 
                             estimation[paste(par[1], comp, essay, sep=".")],
                             estimation[paste(par[2], comp, essay, sep=".")],
                             estimation[paste(par[3], comp, essay, sep=".")],
                             estimation[paste(par[4], comp, essay, sep=".")]),
             IC50=10^estimation[paste(par[3], comp, essay, sep=".")],
             IC50.inf=10^ci[paste(par[3], comp, essay,  sep="."), 1],
             IC50.sup=10^ci[paste(par[3], comp, essay,  sep="."), 2])    
  }
  # graphique
  png(file = paste(output, "/estimated_curves_", comp, ".png", sep=""), width = 500)
  plt <- ggplot(data.comp) + geom_point(aes(x=conc, y=OD, col=essay), alpha=0.8) +
    geom_line(aes(x=conc2, y=curve, col=essay), size=0.7, show.legend = F) +
    ggtitle(paste("Estimated curves and", IC50.label, "for product", comp, sep=" ")) +
    geom_vline(aes(xintercept=IC50, col=essay), size=0.7, show.legend = F) +
    geom_vline(aes(xintercept=IC50.inf, col=essay), linetype="dashed", size=0.7, show.legend = F) +
    geom_vline(aes(xintercept=IC50.sup, col=essay), linetype="dashed", size=0.7, show.legend = F) +
    scale_x_continuous(trans=pseudo_log_trans(sigma=(10^a)/10, base=10), breaks = breaks) +
    ylab(OD.label) + xlab(conc.label) +
    theme_hc() + scale_colour_brewer(palette="Dark2") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "right") +
    guides(colour = guide_legend(override.aes = list(shape=rep(19, nb.essay),
                                                     size=rep(5, nb.essay),
                                                     alpha=rep(1, nb.essay))))
  for (i in 1:nb.essay){
    plt <- plt + 
      annotate("text", x = Inf, y = Inf,
               label = paste(strrep("\n\n", i),
                             IC50.label,
                             " = ",
                             toString(round(filter(data.comp, essay==levels(data$essay)[i])$IC50[1], 4)),
                             " ",
                             conc.unit,
                             "\n",
                             "confidence interval : [",
                             toString(round(filter(data.comp, essay==levels(data$essay)[i])$IC50.inf[1], 4)),
                             ";",
                             toString(round(filter(data.comp, essay==levels(data$essay)[i])$IC50.sup[1], 4)),
                             "]",
                             sep=""),
               hjust = 1, vjust = 1, col=brewer.pal(8, "Dark2")[i])
  }
  plot_list[[comp]] <- plt
  print(plt)
  dev.off()
}
grid.arrange(grobs = plot_list, ncol = 2)
return(plot_list)
}

plot5_calcul_CI50_diff <- function(){
  output <- paste(main.output, "calcul_CI50", sep="/")
# Graphique : convergence des intervalles de confiance (bootstrap)
if (ci.method == "bootstrap"){
  conv <- data.frame(bootstrap.samples=c("convergence", "no convergence"),
                     rate=c(conv.ci/B.boot, (B.boot-conv.ci)/B.boot))
  if(any(conv$rate==0)){
    conv$rate[which(conv$rate==0)] <- NA
  }
  png(file = paste(output, "/convergence_boostrap.png", sep=""), width = 500)
  plt <- ggplot(conv, aes(x="", y=rate, fill=bootstrap.samples)) +
    ggtitle("Convergence for bootstrap confidence intervals for regression parameters") +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start=0) + theme_hc() + scale_fill_brewer(palette="Dark2") +
    xlab("") + labs(fill="bootstrap samples") +
    theme(axis.text.x=element_blank()) +
    geom_text(aes(y = 1-(cumsum(rate)-rate/2) , label = percent(rate)), size=5) +
    theme(plot.title = element_text(hjust = 0.5))
  print(plt)
  dev.off()
  return(plt)

}
}