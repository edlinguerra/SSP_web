# Falta:

# Intrucciones básicas de secuencia de uso
# Nota de cita a la página web, así como limitaciones del servidor y estar atentos al nuevo.
# Agregar Título a gráfico
# Subir a shinyapps.io

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

data.type <- c("P/A", "counts", "cover")
Sest.method <- c("average", "chao", "jack1", "jack2", "boot")
method <- c("jaccard","bray", "manhattan", "euclidean", "canberra", "clark", "kulczynski", "gower", "altGower", "morisita", "horn", "mountford", "raup", "binomial", "chao", "cao", "mahalanobis")
transf <- c('none', 'square root', 'fourth root', 'Log (X+1)', 'P/A')
mult <- c(FALSE, TRUE)

################# User interface
ui <- fluidPage(
  waiter::use_waiter(),
  titlePanel("Online Simulated Sampling Procedure for Community Ecology (SSP-Online)"),
  
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
)

############## Server
server <- function(input, output, session) {
  
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
  
  #assempar  
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
  
  #simdata
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
  
  #sampsd
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
  
  #summary_ssp
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
  
  #ioptimum
  opt <- eventReactive(input$ioptimum, {
    ioptimum(xx = sum.MSE(), multi.site = input$single, c1 = input$c1, c2 = input$c2, c3 = input$c3)
  })
  
  #datquality
  qua <- eventReactive(input$datq, {
    datquality(data = data(), dat.sim = sim(), Par = par(), transformation = input$tran, method = input$method)  
    
  })
  
  #plot_ssp
  plot <- eventReactive(input$ioptimum, {
    plot_ssp(xx = sum.MSE(), opt = opt(), multi.site = input$single)
  })
  
  ##########    outputs
  #Summary output
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
  
  
  ###Quality output
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
        locations = cells_column_labels(columns = vars(S.mean, S.sd, D.mean, D.sd))) %>% 
      tab_footnote(
        footnote = "The range applies only to simulated data.",
        locations = cells_column_labels(columns = vars(MVDmin, MVDmax)))
    
  })
  
  output$download2 <- downloadHandler(
    filename = "quality_mySSP.csv",
    content = function(file) {
      write.csv(qua(), file)
    },
    contentType = "text/csv"
  )
  
  ### Plot output
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)