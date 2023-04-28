library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(Rcpp)
library(scales)
library(ggthemes)
library(RColorBrewer)
library(boot)
library(minpack.lm)
library(doParallel)
library(nlstools)
library(cowplot)
library(shinythemes)


source("preparation_funct.R")
source("statistiques_function.R")

source("stat_func2.R")

source("calcul_CI50_commune.R")
source("sorties_calcul_CI50_commune.R")

source("plots_sorties_calcul_CI50_commune.R")

source("calcul_CI50_diff.R")
source("table_sorties_calcul_CI50_diff.R")
source("sorties_calcul_CI50_diff.R")

source("diagnostic_calcul_CI50.R")

source("comparaison_CI50_commune.R")
source("sorties_comparaison_CI50_commune.R")

source("comparaison_CI50_diff.R")
source("sorties_comparaison_CI50_diff.R")

source("resume_CI50_diff.R")
source("resume_CI50_commune.R")

# Define UI for data upload app ----
ui <- fluidPage(
  
  theme = shinytheme('flatly'),
  
  # App title ----
  titlePanel("Outil informatique d'évaluation de néofongicides bio-sourcés"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      #checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Semicolon = ";",
                               Comma = ",",
                               Tab = "\t"),
                   selected = ";"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      
      tags$hr(),
      
      textInput("conc", "Concentrations (conc):", value = "conc_mg_L"),
      
      textInput("OD", "Densités optiques (OD):", value = "DO"),
      
      textInput("EO", "Composés (EO):", value = "HE"),
      
      textInput("essay", "Manipulations (essay):", value = "manip"),
      
      tags$hr(),
      
      textInput("conc.label", "conc.label :", value = "concentration (mg/L)"),
      textInput("conc.unit", "conc.unit :", value = "mg/L"),
      textInput("OD.label", "OD.label :", value = "optical density"),
      
      tags$hr(),
      
      selectInput("bas0", "bas0 :",
                  choices = c("FALSE", "TRUE")),
      
      
      # Input: Selector for  ----
      selectInput("ci", "CI:",
                  c("CI50" = "CI50",
                    "CI90" = "CI90")),
      
      # Input: Selector for CI50 partagée entre manip ----
      selectInput("same.IC50", "CI50 partagée entre manip (same.IC50):",
                  c("Non" = "Non",
                    "Oui" = "Oui")),
      
      helpText("Sauf cas très exceptionnel, utiliser CI50 partagée."),
      
      
      # Input: Proportion pour la concentration inhibitrice
      # à garder et à utiliser d'une manière conséquente ----
      numericInput(inputId = "prop",
                   label = "Proportion pour la concentration inhibitrice (prop):",
                   value = 50),
      
      helpText("A garder et à utiliser d'une manière conséquente"),
      
      
      # Input: Paramètre graphique à augmenter quand le haut est mal représenté  ----
      numericInput(inputId = "opti.graph",
                   label = "Paramètre graphique (opti.graph):",
                   value = 100),
      
      helpText("Paramètre graphique à augmenter quand le haut est mal représenté"),
      
      
      # Input: Selector for test.global.call ----
      selectInput("test.global.call", "Comparaison globale des CI50 (test.global.call):",
                  c("Oui" = "Oui",
                    "Non" = "Non"), ),
      
      
      tags$hr(),
      
      numericInput("lvl", "Niveau de l'intervalle de confiance:", 0.95, min = 0, max = 1, step = 0.01),
      numericInput("conv.ci.lvl", "Niveau de confiance pour l'intervalle de convergence:", 0.7, min = 0, max = 1, step = 0.01),
      numericInput("risk", "Risque alpha:", 0.05, min = 0, max = 1, step = 0.01),
      selectInput("ci.method", "Méthode pour calculer les IC:",
                  choices = c("bootstrap" = "bootstrap", "Wald" = "Wald"),
                  selected = "bootstrap"),
      numericInput("B.boot", "Nombre de réplications bootstrap:", 10, min = 1, max = 10000, step = 1),
      numericInput("B.perm", "Nombre de permutations:", 1000, min = 1, max = 10000, step = 1),
      selectInput("test.global", "Comparaisons globale:",
                  choices = c("test F" = "test F", "test permutation" = "test permutation"),
                  selected = "test F"),
      textInput("test.pairs", "Test par paires:", "IC"),
      numericInput("maxiter", "Nombre maximal d'itérations:", 50, min = 1, max = 1000, step = 1),
      numericInput("taille.grille", "Taille de la grille:", 11, min = 1, max = 1000, step = 1),
      
      tags$hr(),
      
      textInput("main.output", "Nom du dossier qui va contenir les résultats de l'analyse :", value = ""),
      
      tags$hr(),
      
      actionButton("run", "Lancer les calculs")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Data", tableOutput("table")),
                  tabPanel("Stats", tableOutput("stats")),
                  tabPanel("Plot", plotOutput("plotStat1"),
                           fillCol(plotOutput("plotStat2"),
                                   plotOutput("plotStat3"),),),
                  tabPanel("Calcul_CI50", tableOutput("CI50")),
                  tabPanel("Plots_CI50", plotOutput("plot1_CI50"),
                           plotOutput("plot2_CI50"),
                           plotOutput("plot3_CI50"),
                           uiOutput("plot4_CI50"),
                           plotOutput("plot5_CI50")),
                  tabPanel("Diagnostic", tableOutput("diagnostic")),
                  tabPanel("Comparaison_CI50", tableOutput("comparaison_CI50_tab1"),
                           textOutput("comparaison_CI50_txt"),
                           tableOutput("comparaison_CI50_tab2")),
                  tabPanel("Resume_CI50", tableOutput("resume_CI50_tab"),
                           plotOutput("resume_CI50_plot"))
                  
                  
      )
      
    )
    
  )
)


# Define server logic to read selected file ----
server <- function(input, output) {
  
  observeEvent(input$run, {
    
    req(input$file1)
    
    data <- read.csv(input$file1$datapath,
                     sep = input$sep)
    
    ###############Initialisation des données#######################################
    
    conc <- input$conc
    OD <- input$OD
    EO <- input$EO
    essay <- input$essay
    
    assign("conc", conc, envir = .GlobalEnv)
    assign("OD", OD, envir = .GlobalEnv)
    assign("EO", EO, envir = .GlobalEnv)
    assign("essay", essay, envir = .GlobalEnv)
    
    conc.label <- input$conc.label
    conc.unit <- input$conc.unit
    OD.label <- input$OD.label
    
    assign("conc.label", conc.label, envir = .GlobalEnv)
    assign("conc.unit", conc.unit, envir = .GlobalEnv)
    assign("OD.label", OD.label, envir = .GlobalEnv)
    
    
    if(input$bas0 == "TRUE"){
      assign("bas0", TRUE, envir = .GlobalEnv)
    }else{
      assign("bas0", FALSE, envir = .GlobalEnv)
    }
    
    
    
    assign("statistics.call", TRUE, envir = .GlobalEnv)
    assign("estimation.call", TRUE, envir = .GlobalEnv)
    
    
    
    assign("lvl", input$lvl, envir = .GlobalEnv)
    assign("risk", input$risk, envir = .GlobalEnv)
    assign("conv.ci.lvl", input$conv.ci.lvl, envir = .GlobalEnv)
    assign("ci.method", input$ci.method, envir = .GlobalEnv)
    assign("B.boot", input$B.boot, envir = .GlobalEnv)
    assign("B.perm", input$B.perm, envir = .GlobalEnv)
    assign("test.global", input$test.global, envir = .GlobalEnv)
    assign("test.pairs", input$test.pairs, envir = .GlobalEnv)
    assign("maxiter", input$maxiter, envir = .GlobalEnv)
    assign("taille.grille", input$taille.grille, envir = .GlobalEnv)
    
    opti.graph <- input$opti.graph
    assign("opti.graph", opti.graph, envir = .GlobalEnv)
    
    main.output <- input$main.output
    assign("main.output", main.output, envir = .GlobalEnv)
    
    prop <- input$prop
    assign("prop", prop, envir = .GlobalEnv)
    
    if(input$test.global.call == "Oui"){
      assign("test.global.call", TRUE, envir = .GlobalEnv)
    }else{
      assign("test.global.call", FALSE, envir = .GlobalEnv)
    }
    
    
    if(input$same.IC50 == "Oui"){
      assign("same.IC50", TRUE, envir = .GlobalEnv)
    }else{
      assign("same.IC50", FALSE, envir = .GlobalEnv)
    }
    
    ################################################################################        
    
    ###############Preparation des données##########################################
    
    data <- preparation_data(data, conc, OD, EO, essay, nb.EO, nb.essay)
    
    assign("nb.EO", length(levels(data$EO)), envir = .GlobalEnv)
    assign("nb.essay", length(levels(data$essay)), envir = .GlobalEnv)
    
    assign("data", data, envir = .GlobalEnv)
    
    ################################################################################    
    
    
    # DOSSIERS QUI VONT CONTENIR LES RESULTATS :
    # si le dossier n'existe pas : création du dossier
    if (!dir.exists(main.output)){ 
      dir.create(main.output)
    }   
    
    if (!dir.exists(paste(main.output, "statistiques_descriptives", sep="/"))){
      dir.create(paste(main.output, "statistiques_descriptives", sep="/"))
    }
    
    # Régression
    if (!dir.exists(paste(main.output, "calcul_CI50", sep="/"))){
      dir.create(paste(main.output, "calcul_CI50", sep="/"))
    }
    # Tests
    if (!dir.exists(paste(main.output, "comparaison_CI50", sep="/"))){
      dir.create(paste(main.output, "comparaison_CI50", sep="/"))
    }
    # Résumé
    if (!dir.exists(paste(main.output, "resume", sep="/"))){
      dir.create(paste(main.output, "resume", sep="/"))
    }
    
    
    output$table <- renderTable({
      ###############Affichage des données############################################    
      
      if(input$disp == "head") {
        return(head(data))
      }
      else {
        return(data)
      }
      
      ################################################################################    
      
    })
    
    stat <- statistic(data)
    assign("stat", stat, envir = .GlobalEnv)
    
    output$stats <- renderTable({
      return(stat)
    })
    
    pltStat1 <- statistic2(data)
    assign("pltStat1", pltStat1, envir = .GlobalEnv)
    
    output$plotStat1 <- renderPlot({
      
      return(pltStat1)
    })
    
    pltStat2 <- statistic3(data)
    assign("pltStat2", pltStat2, envir = .GlobalEnv)
    
    output$plotStat2 <- renderPlot({
      
      return(pltStat2)
    })
    
    pltStat3 <- statistic4(data)
    assign("pltStat3", pltStat3, envir = .GlobalEnv)
    
    output$plotStat3 <- renderPlot({
      
      return(pltStat3)
    })
    
    IC50.label <- paste("IC", prop, sep="")
    assign("IC50.label", IC50.label, envir = .GlobalEnv)
    
    logIC50.label <- paste("logIC", prop, sep="")
    assign("logIC50.label", logIC50.label, envir = .GlobalEnv)
    
    if(same.IC50 == TRUE){
      calcul_CI50_commune(data, bas0, taille.grille, nb.essay, B.boot, lvl, conv.ci.lvl)
      Table_sorties_calcul_CI50 <- Table_sorties_calcul_CI50_commune(data, par, EO, OD, essay, ci, estimation)
    }else{
      calcul_CI50_diff(data, bas0, taille.grille, nb.essay, B.boot, lvl, conv.ci.lvl)
      Table_sorties_calcul_CI50 <- Table_sorties_calcul_CI50_diff(data.essay, par, EO, OD, essay)
    }
    
    output$CI50 <- renderTable({
      
        return(Table_sorties_calcul_CI50)
      
    })
    
    if(same.IC50 == TRUE ){
      plot1_calcul_CI50 <- plot1_calcul_CI50_commune()
      plot2_calcul_CI50 <- plot2_calcul_CI50_commune()
      plot3_calcul_CI50 <- plot3_calcul_CI50_commune()
      plt_list <- plot4_calcul_CI50_commune()
      plot5_calcul_CI50 <- plot5_calcul_CI50_commune()
    }else{
      plot1_calcul_CI50 <- plot1_calcul_CI50_diff()
      plot2_calcul_CI50 <- plot2_calcul_CI50_diff()
      plot3_calcul_CI50 <- plot3_calcul_CI50_diff()
      plt_list <- plot4_calcul_CI50_diff()
      plot5_calcul_CI50 <- plot5_calcul_CI50_diff()
    }
    
    output$plot1_CI50 <- renderPlot({
      return(plot1_calcul_CI50)
    })
    
    output$plot2_CI50 <- renderPlot({
      return(plot2_calcul_CI50)
    })
    
    output$plot3_CI50 <- renderPlot({
      return(plot3_calcul_CI50)
    })
    
    output$plot4_CI50 <- renderUI({
        
        plot_output_list <- lapply(seq_along(plt_list), function(i) {
          plot_output <- plotOutput(paste0("plot", i))
          output[[paste0("plot", i)]] <- renderPlot({
            plt_list[[i]]
          })
          return(plot_output)
        })
        do.call(div, c(list(id = "all_plots"), plot_output_list))
        
    })
    
    output$plot5_CI50 <- renderPlot({
      return(plot5_calcul_CI50)
    })
    
    diagnostic_calcul_CI50 <- diagnostic_calcul_CI50()
    
    output$diagnostic <- renderTable({
      return(diagnostic_calcul_CI50)
    })
    
    if(same.IC50 == TRUE ){
      comparaison_CI50_commune()
      Table1_comparaison_CI50 <- Table1_comparaison_CI50_commune()
      Table2_comparaison_CI50 <- Table2_comparaison_CI50_commune()
    }else{
      comparaison_CI50_diff()
      Table1_comparaison_CI50 <- Table1_comparaison_CI50_diff()
      Table2_comparaison_CI50 <- Table2_comparaison_CI50_diff()
    }
    
    output$comparaison_CI50_tab1 <- renderTable({
      return(Table1_comparaison_CI50)
    })
    
    output$comparaison_CI50_txt <- renderText({
      a <- ""
      for(i in seq_along(pval.global)){
        a <- a %>% paste("pvalue ", i, " = ", pval.global[[i]], "///")
      }
      return(a)
    })
    
    output$comparaison_CI50_tab2 <- renderTable({
      if(same.IC50 == TRUE ){
        return(Table2_comparaison_CI50_commune())
      }else{
        return(Table2_comparaison_CI50_diff())
      }
    })
    
    if (nb.EO == 1){
      test.pairs.done <- FALSE
    }
    
    if(same.IC50 == TRUE ){
      resume_CI50_tab <- resume_CI50_commune_tab()
      resume_CI50_plot <- resume_CI50_commune_plot()
    }else{
      resume_CI50_tab <- resume_CI50_diff_tab()
      resume_CI50_plot <- resume_CI50_diff_plot()
    }
    
    output$resume_CI50_tab <- renderTable({
      return(resume_CI50_tab)
    })
    
    output$resume_CI50_plot <- renderPlot({
      return(resume_CI50_plot)
    })
  })
}

# Create Shiny app ----
shinyApp(ui, server)