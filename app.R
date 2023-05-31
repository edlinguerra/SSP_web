library(shiny)
library(vroom)
library(SSP)
library(sampling)
library(vegan)
library(DT)
library(waiter)
library(gt)
library(dplyr)
library(shinyWidgets)
library(markdown)
library(tidyr)
library(ecocbo)
library(shinycssloaders)


data.type <- c("P/A", "counts", "cover")
Sest.method <- c("average", "chao", "jack1", "jack2", "boot")
method <- c("jaccard","bray", "manhattan", "euclidean", "canberra", "clark", "kulczynski", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial", "chao", "cao", "mahalanobis")
transf <- c('none', 'square root', 'fourth root', 'Log (X+1)', 'P/A')
mult <- c(FALSE, TRUE)
pickPlotNames <- c("Power plot", "Density plot", "Both")
pickPlotValues <- c("power", "density", "both")
pickcboNames <- c("Budget", "Precision")
pickcboValues <- c("budget", "precision")

################# User interface
ui <- fluidPage(
  waiter::use_waiter(),
  titlePanel("ecosimverse"),
  
  fluidPage(
    # intro ----
    tabsetPanel(
      tabPanel(
        title = "Introduction",
        includeMarkdown("SSP_online.md")
      ),
      
      # SSP ----
      tabPanel(
        title = "Online Simulated Sampling Procedure for Community Ecology (SSP-Online)",
        sidebarLayout(
          sidebarPanel(
            wellPanel(p(strong("1. assempar")),
                      fileInput("file", "Pilot data", accept = ".csv"),
                      selectInput("type", "Nature of the data to be processed", data.type),
                      selectInput("Sest", "Method for estimating species richness", Sest.method),
                      actionButton("parameters", "GO assempar")
            ),
            wellPanel(p(strong("2. simdata")),
                      sliderInput("cases", "Cases", 1, 100, 1),
                      sliderInput("sites", "Sites", 1, 100, 1),
                      sliderInput("N", "N", 1, 100, 1),
                      actionButton("simul", "GO simdata")
            ),
            wellPanel(p(strong("3. sampsd")) ,  
                      sliderInput("m", "m", 1, 100, 1),
                      sliderInput("n", "n", 1, 100, 1),
                      sliderInput("k", "k", 1, 100, 1),
                      selectInput("method", "Dissimilarity", method),
                      selectInput("tran", "Transformation", transf),
                      actionButton("samp", "GO sampsd"),
            ),
            wellPanel(p(strong("4. summary_ssp & datquality")) ,
                      radioButtons("single", "Several sites simulated", mult),
                      actionButton("summ", "GO summary_ssp"),
                      actionButton("datq", "GO datquality")
            ),
            wellPanel(p(strong("5. ioptimum & plot")),
                      numericInput("c1", "First cut %", value = 10, min = 10, max = 50),
                      numericInput("c2", "Second cut %", value = 5, min = 5, max = 20),
                      numericInput("c3", "Third cut %", value = 3, min = 0, max = 5),
                      radioButtons("var3", label = "Select the file type", choices = list("png", "pdf")),
                      actionButton("ioptimum", "GO ioptimum")
            )),
          
          mainPanel(
            tabsetPanel(
              tabPanel("Info", 
                       includeMarkdown("SSP_online.md")),
              tabPanel("Summary MultSE", 
                       dataTableOutput("summary"),
                       downloadButton("download1")),
              tabPanel("Quality", gt_output("quality"),
                       downloadButton("download2")),
              tabPanel("MultSE Plot", plotOutput("plot"),
                       downloadButton("download3"))
            )
          )
        )
      ),
      
      # ecocbo ----
      tabPanel(
        title = "Online Simulated Ecological Cost-Benefit Optimization (ecocbo-Online)",
        sidebarLayout(
          sidebarPanel(
            wellPanel(p(strong("0. Prepare the data")),
                      fileInput("file_cbo", "Pilot data", accept = ".csv")                
            ),
            wellPanel(p(strong("1. assempar")),
                      selectInput("type_cbo", "Nature of the data to be processed", data.type, selected = "counts"),
                      selectInput("Sest_cbo", "Method for estimating species richness", Sest.method),
                      actionButton("parameters_cbo", "GO assempar")
            ),
            wellPanel(p(strong("2. simdata H0")),
                      sliderInput("cases_cbo0", "Cases", 1, 100, 5),
                      sliderInput("sites_cbo0", "Sites", 1, 1, 1),
                      sliderInput("N_cbo0", "N", 1, 1000, 1000),
                      actionButton("simul_cbo0", "GO simdata")
            ),
            wellPanel(p(strong("3. simdata Ha")),
                      sliderInput("cases_cboa", "Cases", 1, 100, 5),
                      sliderInput("sites_cboa", "Sites", 1, 1000, 10),
                      sliderInput("N_cboa", "N", 1, 1000, 100),
                      actionButton("simul_cboa", "GO simdata")
            ),
            wellPanel(p(strong("4. sim_beta")),
                      sliderInput("m_cbo", "m", 2, 100, 4),
                      sliderInput("n_cbo", "n", 2, 100, 7),
                      sliderInput("k_cbo", "k", 2, 100, 4),
                      sliderInput("alpha_cbo", "alpha", 0, 1, 0.05),
                      selectInput("method_cbo", "Dissimilarity", method, selected = "bray"),
                      selectInput("transf_cbo", "Transformation", transf),
                      radioButtons("dummy_cbo", "Is a dummy variable needed?", mult, 
                                   selected = "FALSE", inline = TRUE),
                      radioButtons("parall_cbo", "Use parallel computing?", mult,
                                   selected = "TRUE", inline = TRUE),
                      actionButton("betae_cbo", "GO simbeta")
            ),
            wellPanel(p(strong("5. plot_power")),
                      radioButtons("nPlot_yes", "Pick n?", mult,
                                   selected = "TRUE", inline = TRUE),
                      sliderInput("mPlot_cbo", "m", 2, 100, 4),
                      sliderInput("nPlot_cbo", "n", 2, 100, 4),
                      radioButtons("pickPlot_cbo", "Type of plot", 
                                   choiceNames = pickPlotNames,
                                   choiceValues = pickPlotValues,
                                   selected = "both", inline = TRUE),
                      actionButton("plot_cbo", "GO plot_power")
            ),
            wellPanel(p(strong("6. scompvar & sim_cbo")),
                      radioButtons("pickcbo", "Type of optimization",
                                   choiceNames = pickcboNames,
                                   choiceValues = pickcboValues,
                                   selected = "budget", inline = TRUE),
                      numericInput("multSE_cbo", "Multivariate standard error", 
                                   value = 0.20),
                      numericInput("ct_cbo", "Total cost", value = 20000),
                      numericInput("ck_cbo", "Sampling unit cost", value = 1200),
                      numericInput("cj_cbo", "Sample cost", value = 400),
                      actionButton("cbo_cbo", "GO sim_cbo")
            )
          ),
          
          mainPanel(
            tabsetPanel(
              tabPanel("H0",
                       tableOutput("dataH0_Out")),
              tabPanel("Ha",
                       tableOutput("dataHa_Out")),
              tabPanel("parH0",
                       tableOutput("parH0_Out")),
              tabPanel("parHa",
                       tableOutput("parHa_Out")),
              tabPanel("simH0",
                       tableOutput("simH0_Out")),
              tabPanel("simHa",
                       tableOutput("simHa_Out")),
              tabPanel("betaE",
                       withSpinner(tableOutput("power_Out"), type = 1)),
              tabPanel("plot",
                       withSpinner(plotOutput("plot_Out"), type = 1)),
              tabPanel("CompVar",
                       withSpinner(tableOutput("CV_Out"), type = 1)),
              tabPanel("CBO",
                       withSpinner(tableOutput("cbo_Out"), type = 1))
            )
          )
        )
      )
    )
  )
)


############## Server
server <- function(input, output, session) {
  
  # functions SSP ----
  data <- reactive({
    infile <- input$file
    #if (is.null(infile)) {
    # User has not uploaded a file yet
    #   return(NULL)
    #}
    # ext <- tools::file_ext(infile$datapath)
    #       switch(ext,
    #           csv = vroom::vroom(input$file$datapath, delim = ","),
    #           validate("Invalid file; Please upload a .csv file"))
    read.csv(infile$datapath)
  })
  
  ## assempar  ----
  par <- eventReactive(input$parameters, {
    assempar(data(), type = input$type, Sest.method = input$Sest)
  })
  
  
  observeEvent(input$parameters, {
    progress <- Progress$new(min =1, max = 10)
    on.exit(progress$close())
    progress$set(message = "estimating parameters",
                 detail = "please wait...")
    for (i in seq_len(10)) {
      progress$inc(1)
    }
    assempar(data(), type = input$type, Sest.method = input$Sest)
  })
  
  ## simdata ----
  sim <- eventReactive(input$simul, {
    simdata(Par = par(), cases = input$cases, N = input$N , sites = input$sites)
  })
  
  observeEvent(input$simul, {
    progress <- Progress$new(min =1, max = input$cases)
    on.exit(progress$close())
    progress$set(message = "simulating data",
                 detail = "please wait...")
    for (i in seq_len(input$cases)) {
      progress$inc(1)
    }
    simdata(Par = par(), cases = input$cases, N = input$N , sites = input$sites)
  })
  
  ## sampsd----
  mse <- eventReactive(input$samp, { 
    sampsd(dat.sim = sim(), Par = par(), transformation = input$tran, method = input$method, n = input$n, m = input$m, k = input$k)
  })
  
  observeEvent(input$samp, { 
    progress <- Progress$new(min= 1, max = input$cases)
    on.exit(progress$close())
    progress$set(message = "sampling and estimating MultSE",
                 detail= "please wait, this may take a while ...")
    for (i in seq_len(input$cases)) {
      progress$inc(1)
    }
    sampsd(dat.sim = sim(), Par = par(), transformation = input$tran, method = input$method, n = input$n, m = input$m, k = input$k)
  })
  
  ## summary_ssp ----
  sum.MSE <- eventReactive(input$summ, { 
    summary_ssp(results = mse(), multi.site = input$single)})
  
  observeEvent(input$summ, { 
    progress <- Progress$new(min= 1, max = 100)
    on.exit(progress$close())
    progress$set(message = "calculating summary",
                 detail= "please wait...")
    for (i in seq_len(100)) {
      progress$inc(1)
    }
    summary_ssp(results = mse(), multi.site = input$single)
  })
  
  ## ioptimum ----
  opt <- eventReactive(input$ioptimum, {
    ioptimum(xx = sum.MSE(), multi.site = input$single, c1 = input$c1, c2 = input$c2, c3 = input$c3)
  })
  
  ## datquality ----
  qua <- eventReactive(input$datq, {
    datquality(data = data(), dat.sim = sim(), Par = par(), transformation = input$tran, method = input$method)  
    
  })
  
  ## plot_ssp ----
  plot <- eventReactive(input$ioptimum, {
    plot_ssp(xx = sum.MSE(), opt = opt(), multi.site = input$single)
  })
  
  # functions ecocbo ----
  ## read data ecocbo ----
  data_cbo <- reactive({
    infile <- input$file_cbo
    read.csv(infile$datapath)
  })
  
  ## simmulate H0 and Ha ----
  dataH0 <- reactive({
    temp <- data_cbo()
    temp$site <- "T0"
    return(temp)
  })
  dataHa <- reactive({
    data_cbo()
  })
  
  ## assempar ecocbo ----
  par_cbo <- eventReactive(input$parameters_cbo, {
    progress <- Progress$new(min = 1, max = 20)
    on.exit(progress$close())
    progress$set(message = "estimating parameters",
                 detail = "please wait...")
    for(i in seq_len(20)){
      progress$inc(1)
    }
    
    temp <- list(H0 = dataH0(), Ha = dataHa())
    lapply(temp, assempar, type = input$type_cbo, Sest.method = input$Sest_cbo)
  })
  
  ## simdata ecocbo ----
  sim_cbo0 <- eventReactive(input$simul_cbo0, {
    progress <- Progress$new(min = 1, max = input$cases_cbo0 * input$N_cbo0 * input$sites_cbo0)
    on.exit(progress$close())
    progress$set(message = "simulating dataH0",
                 detail = "please wait...")
    for(i in seq_len(input$cases_cbo0 * input$N_cbo0 * input$sites_cbo0)){
      progress$inc(1)
    }
    
    simdata(Par = par_cbo()$H0, cases = input$cases_cbo0, 
            N = input$N_cbo0, sites = input$sites_cbo0)
  })
  sim_cboa <- eventReactive(input$simul_cboa, {
    progress <- Progress$new(min = 1, max = input$cases_cboa * input$N_cboa * input$sites_cboa)
    on.exit(progress$close())
    progress$set(message = "simulating dataHa",
                 detail = "please wait...")
    for(i in seq_len(input$cases_cboa * input$N_cboa * input$sites_cboa)){
      progress$inc(1)
    }
    
    simdata(Par = par_cbo()$Ha, cases = input$cases_cboa,
            N = input$N_cboa, sites = input$sites_cboa)
  })
  
  ## simbeta ----
  # update the possible values for n and m in the next step
  observeEvent(sim_cboa(), {
    updateSliderInput(session, inputId = "m_cbo", max = input$sites_cboa)
    updateSliderInput(session, inputId = "n_cbo", max = input$N_cboa)
  })
  
  beta_cbo <- eventReactive(input$betae_cbo, {
    progress <- Progress$new(min = 1, max = input$n_cbo * input$m_cbo * input$k_cbo)
    on.exit(progress$close())
    progress$set(message = "calculating",
                 detail = "please wait...")
    for (i in seq_len(input$n_cbo * input$m_cbo * input$k_cbo)) {
      progress$inc(1)
    }
    
    sim_beta(simH0 = sim_cbo0(), 
             simHa = sim_cboa(),
             n = input$n_cbo,
             m = input$m_cbo,
             k = input$k_cbo,
             alpha = input$alpha_cbo,
             transformation = input$transf_cbo,
             method = input$method_cbo,
             dummy = input$dummy_cbo,
             useParallel = input$parall_cbo)
  })
  pwr <- reactive({
    req(beta_cbo())
    temp <- beta_cbo()$Power[,c(1:3)] %>%
      tidyr::pivot_wider(names_from = "n", values_from = "Power",
                         names_prefix = "n =") %>%
      mutate(m = paste0("m = ", m))
  })
  
  ## plot_power ----
  #update possible values for n and m in the next step
  observeEvent(beta_cbo(), {
    updateSliderInput(session, inputId = "nPlot_cbo", max = input$n_cbo)
    updateSliderInput(session, inputId = "mPlot_cbo", max = input$m_cbo)
  })
  
  nn <- eventReactive(input$plot_cbo, {
    if(input$nPlot_yes == "TRUE"){input$nPlot_cbo} else{NULL}
  })
  
  plot_cbo <- eventReactive(input$plot_cbo, {
    plot_power(data = beta_cbo(), 
               n = nn(), 
               m = input$mPlot_cbo, 
               method = input$pickPlot_cbo)
  })
  
  ## scompvar ----
  CV_cbo <- eventReactive(input$cbo_cbo, {
    scompvar(data = beta_cbo())
  })
  
  ## sim_cbo ----
  cbo_cbo <- eventReactive(input$cbo_cbo, {
    if(input$pickcbo == "budget"){
      sim_cbo(CV_cbo(), multSE = NULL, 
              ct = input$ct_cbo,
              ck = input$ck_cbo,
              cj = input$cj_cbo)
    } else {
      sim_cbo(CV_cbo(), multSE = input$multSE_cbo,
              ct = NULL,
              ck = input$ck_cbo,
              cj = input$cj_cbo)
    }
  })
  
  # outputs SSP ----
  ## Summary output ----
  output$summary <- renderDataTable({
    
    sum.MSE() %>% 
      mutate(mean = round(mean, 2), upper = round(upper, 2),
             lower = round(lower, 2), rel = round(rel, 2),
             der = round(der, 2))
    
  })
  
  output$download1 <- downloadHandler(
    filename = "summary_mySSP.csv",
    content = function(file) {
      write.csv(sum.MSE(), file)
    },
    contentType = "text/csv"
  )
  
  
  ## Quality output ----
  output$quality <- render_gt({
    qua2 <- round(qua(), 2)
    colnames(qua2) <- c("S.mean", "S.sd", "D.mean", "D.sd", "MVDmin","MVDmax") 
    qua2 %>%
      mutate("Data" = c("Pilot", "Simulated")) %>% 
      select(c(7,1:6)) %>% 
      gt () %>%
      tab_header(
        title = "Quality of simulation",
        subtitle = "Comparison of pilot data with simulated data") %>% 
      tab_spanner(label = "Species", columns = matches("S.mean|S.sd")) %>%
      tab_spanner(label = "Simpson Diversity index", columns = matches("D.mean|D.sd")) %>% 
      tab_spanner(label = "Total multivariate dispersion", columns = matches("MVDmin|MVDmax")) %>% 
      cols_label(S.mean = "Mean", S.sd = "SD", "D.mean" = "Mean", "D.sd" = "SD", MVDmin = "Minima", MVDmax = "Maxima") %>%
      cols_align(align = "center") %>% 
      tab_footnote(
        footnote = "Values per sample unit.",
        locations = cells_column_labels(columns = c(S.mean, S.sd, D.mean, D.sd))) %>% 
      tab_footnote(
        footnote = "The range applies only to simulated data.",
        locations = cells_column_labels(columns = c(MVDmin, MVDmax)))
    
  })
  
  output$download2 <- downloadHandler(
    filename = "quality_mySSP.csv",
    content = function(file) {
      write.csv(qua(), file)
    },
    contentType = "text/csv"
  )
  
  ## Plot output ----
  output$plot <- renderPlot(plot())  
  
  output$download3 <- downloadHandler(
    filename = function() {
      paste0("plot_mySSP.", input$var3)
    },
    content = function(file) {
      if(input$var3 == "png")
        png(file) # open the png device
      else
        pdf(file) # open the pdf device
      # plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl()) # draw the plot
      print(plot()) # for GGPLOT
      dev.off()  # turn the device off
    }
  )
  
  # outputs ecocbo ----
  ## read data ecocbo ----
  output$dataH0_Out <- renderTable(dataH0())
  output$dataHa_Out <- renderTable(dataHa())
  
  ## assempar ecocbo ----
  output$parH0_Out <- renderTable(par_cbo()$H0$par)
  output$parHa_Out <- renderTable(par_cbo()$Ha$par)
  
  ## simdata ecocbo ----
  output$simH0_Out <- renderTable(sim_cbo0()[[1]])
  output$simHa_Out <- renderTable(sim_cboa()[[1]])
  
  ## sim_beta ---- 
  output$power_Out <- renderTable(pwr())
  
  ## plot_power ----
  output$plot_Out <- renderPlot(plot_cbo())
  
  ## scompvar ---
  output$CV_Out <- renderTable(CV_cbo())
  
  ## sim_cbo ----
  output$cbo_Out <- renderTable(cbo_cbo())
  
}

# Run the application 
shinyApp(ui = ui, server = server)