Table_sorties_calcul_CI50_commune <- function(data, par, EO, OD, essay, ci, estimation){
output <- paste(main.output, "calcul_CI50", sep="/")
# Tableau des r?sultats {input data par EO OD essay ci estimation }
result <- data %>% group_by(EO, essay) %>% summarise(mean=mean(OD)) %>% select(EO, essay)
for (p in par){
  result <- result %>% mutate(
    par.inf=ci[paste(p, EO, essay,  sep="."), 1],
    par=estimation[paste(p, EO, essay,  sep=".")],
    par.sup=ci[paste(p, EO, essay,  sep="."), 2],
    par.width=ci[paste(p, EO, essay,  sep="."), 2]-ci[paste(p, EO, essay,  sep="."), 1])
  colnames(result)[colnames(result) %in% c("par.inf", "par", "par.sup", "par.width")] <- paste(p, c(".inf", "", ".sup", ".width"), sep="")
}
result <- result %>% mutate(IC50.inf=10^logIC50.inf,
                            IC50=10^logIC50,
                            IC50.sup=10^logIC50.sup)
result <- result %>% mutate(IC50.width=IC50.sup-IC50.inf)
colnames(result) <- gsub("IC50", paste("IC", prop, sep=""), colnames(result))
colnames(result) <- gsub("logIC50", paste("logIC", prop, sep=""), colnames(result))
write.csv2(result, file=paste(output, "regression.csv", sep="/"), row.names=F)

assign("result", result, envir =  .GlobalEnv)

return(result)
#### Tab des resultats ---> result 
}

