resume_CI50_commune_tab <- function(){
  output <- paste(main.output, "resume", sep="/")
  # Table contenant les CI50 estim?es, les intervalles de confiance et les groupes pour
  # chaque produit
  colnames(result) <- gsub(paste("IC", prop, sep=""), "IC50", colnames(result))
  data.summary <- result %>% 
    filter(essay == levels(data$essay)[1]) %>% 
    select(EO, IC50.inf, IC50, IC50.sup)
  if (test.pairs.done & test.pairs == "IC"){
  data.summary$IC50.inf <- 10^tab.group$logIC50.inf
  data.summary$IC50.sup <- 10^tab.group$logIC50.sup
  }
  if (nb.EO > 1){
  data.summary$group <- tab.group$group
  }
  
  if (!IC.done){
    data.summary <- data.summary %>% select(-IC50.inf, -IC50.sup)
  }
  
  if (nb.EO > 1){
  if (!test.pairs.done){
    data.summary <- data.summary %>% select(-group)
  }else{
    if ((ci.method == "bootstrap")&(test.pairs == "IC")){
      if (!IC.done){
        data.summary <- data.summary %>% select(-group)
      }
    }
  }
  }
  colnames(data.summary) <- gsub("IC50", paste("IC", prop, sep=""), colnames(data.summary))
  assign("data.summary", data.summary, envir = .GlobalEnv)
  write.csv2(data.summary, file=paste(output, "resume.csv", sep="/"), row.names=F)
  return(data.summary)
}  
resume_CI50_commune_plot <- function(){
  output <- paste(main.output, "resume", sep="/")
# Graphique
colnames(data.summary) <- gsub(paste("IC", prop, sep=""), "IC50", colnames(data.summary))
a <- floor(log10(min(data.summary[, c("IC50", "IC50.sup", "IC50.inf")])))
b <- ceiling(log10(max(data.summary[, c("IC50", "IC50.sup", "IC50.inf")]))) 
breaks <- c(0, 10^(a:b))
data.summary$legend.ic <- c("confidence interval", rep(NA, nb.EO-1))
png(file = paste(output, "/resume.png", sep=""), width = 500)
plt <- ggplot(data.summary) + geom_col(aes(x=EO, y=IC50, fill=EO), width = 0.7) + 
  ggtitle(paste("Evaluation (with 95% confidence intervals) \n and comparison of", IC50.label, sep=" ")) +
  theme_hc() + scale_fill_brewer(palette = "Dark2") + xlab("") + labs(fill="product") +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  ylab(paste("IC", prop, " ", "(", conc.unit, ")", sep="")) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="right") +
  scale_y_continuous(trans=pseudo_log_trans(sigma=(10^a)/10, base=10), breaks = breaks)
if (IC.done){
  plt <- plt + geom_errorbar(aes(x=EO, ymin=IC50.inf, ymax=IC50.sup), width=0.2) 
}
if (test.pairs.done){
  if ((ci.method == "bootstrap")&(test.pairs == "IC")){
    if (IC.done){
      plt <- plt + geom_text(aes(x=EO, y=sqrt(IC50*10^a/10), label=group))
    }
  }else{
    plt <- plt + geom_text(aes(x=EO, y=sqrt(IC50*10^a/10), label=group))
  }
}
print(plt)
dev.off()
return(plt)
}
