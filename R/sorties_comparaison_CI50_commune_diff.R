# Comparaison globale
if (test.global.call){
  if (test.global.done){
    # pvalue 
    write.csv2(data.frame(pvalue=pval.global), file=paste(output, "comparaison_globale.csv", sep="/"), row.names=F)
  }
}

# Comparaisons par paires
if (test.pairs.done){
  # Table pvalues (comparaisons par paires)
  tab.pval <- table(levels(data$EO), levels(data$EO))
  tab.pval <- tab.pval*NA
  for (i in 1:(nb.EO-1)){
    for (j in (i+1):nb.EO)
      tab.pval[i, j] <- pval.pairs[paste(levels(data$EO)[i], levels(data$EO)[j], sep=".")]
  }
  # Table groupes (comparaisons par paires)
  tab.group <- LetterDisplay(tab.pval, risk/nb.pairs)
  tab.group <- apply(as.matrix(tab.group[, -ncol(tab.group)]), 1, function(x) do.call(paste, c(as.list(x),list(sep=""))))
  tab.group <- data.frame(EO=levels(data$EO),
                          group=tab.group)
  if (test.pairs == "IC"){
    tab.group$logIC50.inf=ci.test[, 1]
    tab.group$logIC50.sup=ci.test[, 2]
  }
  colnames(tab.group) <- gsub("logIC50", paste("logIC", prop, sep=""), colnames(tab.group))
  write.csv2(tab.group, file=paste(output, "comparaisons_paires.csv", sep="/"), na="", row.names=F)
}