resume_CI50_diff_tab <- function(){
  output <- paste(main.output, "resume", sep="/")
# Table contenant les CI50 estim?es, les intervalles de confiance et les groupes pour
# chaque produit
colnames(result) <- gsub(paste("IC", prop, sep=""), "IC50", colnames(result))
data.summary <- result %>% 
  arrange(essay) %>%
  select(essay, EO, IC50.inf, IC50, IC50.sup)
if (test.pairs.done & test.pairs == "IC"){
  tab.group <- subset(tab.group,!is.na(group.defined.by.essay))
  data.summary$IC50.inf <- 10^tab.group$logIC50.inf
  data.summary$IC50.sup <- 10^tab.group$logIC50.sup
}
if (nb.EO > 1){
data.summary$group.defined.by.essay <- tab.group$group.defined.by.essay
if (!IC.done){
  data.summary <- data.summary %>% select(-IC50.inf, -IC50.sup)
}
if (!test.pairs.done){
  data.summary <- data.summary %>% select(-group.defined.by.essay)
}else{
  if ((ci.method == "bootstrap")&(test.pairs == "IC")){
    if (!IC.done){
      data.summary <- data.summary %>% select(-group.defined.by.essay)
    }
  }
}
}
colnames(data.summary) <- gsub("IC50", paste("IC", prop, sep=""), colnames(data.summary))

assign("data.summary", data.summary, envir = .GlobalEnv)

write.csv2(data.summary, file=paste(output, "resume.csv", sep="/"), row.names=F)
return(data.summary)
}

resume_CI50_diff_plot <- function(){
  output <- paste(main.output, "resume", sep="/")
# Remplacer le nom group.defined.by.essay par group,
# pour que le programme suivant puisse utiliser le nom group.
if (nb.EO > 1){
colnames(data.summary) <- sub("group.defined.by.essay", "group", colnames(data.summary))
}

# Graphique
colnames(data.summary) <- gsub(paste("IC", prop, sep=""), "IC50", colnames(data.summary))
a <- floor(log10(min(data.summary[, c("IC50", "IC50.sup", "IC50.inf")])))
b <- ceiling(log10(max(data.summary[, c("IC50", "IC50.sup", "IC50.inf")]))) 
breaks <- c(0, 10^(a:b))
data.summary$legend.ic <- c("confidence interval", rep(NA, nb.EO*nb.essay-1))
png(file = paste(output, "/resume.png", sep=""), width = 500)
plt <- ggplot(data.summary) + geom_col(aes(x=EO, y=IC50, fill=essay), width = 0.7,
                                       position="dodge") + 
  ggtitle(paste("Evaluation (with 95% confidence intervals) \n and comparison of", IC50.label, "separately by essay", sep=" ")) +
  theme_hc() + scale_fill_brewer(palette = "Dark2") + labs(fill="essay") + xlab("product") +
  ylab(paste("IC", prop, " ", "(", conc.unit, ")", sep="")) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="right") +
  scale_y_continuous(trans=pseudo_log_trans(sigma=(10^a)/10, base=10), breaks = breaks)
if (IC.done){
  plt <- plt + geom_errorbar(aes(x=EO, ymin=IC50.inf, ymax=IC50.sup, group=essay), width=0.2, position=position_dodge(width=0.7))
}
if (test.pairs.done){
  if ((ci.method == "bootstrap")&(test.pairs == "IC")){
    if (IC.done){
      plt1 <- plt + geom_text(aes(x=EO, y=sqrt(IC50*10^a/10), label=group, group=essay), position=position_dodge(width=0.7))
    }
  }else{
    plt1 <- plt + geom_text(aes(x=EO, y=sqrt(IC50*10^a/10), label=group, group=essay), position=position_dodge(width=0.7))
  }
}
print(plt1)
dev.off()
return(plt1)
}
