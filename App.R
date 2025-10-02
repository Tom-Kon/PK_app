if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  tryCatch({
    setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  }, error = function(e) {
    setwd(normalizePath("."))
  })
} else {
  setwd(normalizePath("."))
}

source("Libraries and notes.R")
source("Helper functions.R")
source("UI.R")
source("Immediate release.R")
source("Sustained release.R")
source("Sustained release ODE.R")
source("Additions and final steps.R")
source("Plots.R")

ui <- fluidPage(
  withMathJax(),   # <---- This enables LaTeX rendering
  UIFunc()
)

server <- function(input, output, session) {
  simulate_model <- reactive({
    
  req(input$simulateImmediate || input$simulateSustained || input$simulateSustainedODE)
  GI_sus_list <- list()
  B_sus_list <- list()
  GI_imm_list <- list()
  B_imm_list <- list()
  susResults <- list()
  immResults <- list()
  GI_sus_listODE <- list()
  B_sus_listODE <- list()
  
  if(input$simulateSustained) {
    susResults <- SustFunction(input)
    GI_sus_list <- susResults$GI_sus_list
    B_sus_list <- susResults$B_sus_list
  }
  
  if(input$simulateSustainedODE) {
    susResultsODE <- SustFunctionODE(input)
    GI_sus_listODE <- susResultsODE$GI_sus_list
    B_sus_listODE <- susResultsODE$B_sus_list
  }
  
  if(input$simulateImmediate){
    immResults <- ImmFunction(input)
    GI_imm_list <- immResults$GI_imm_list
    B_imm_list <- immResults$B_imm_list
  } 
  
  t <- NULL
  if (input$simulateSustained && length(GI_sus_list) > 0)       t <- GI_sus_list[[1]]$x
  if (is.null(t) && input$simulateSustainedODE && length(GI_sus_listODE) > 0) t <- GI_sus_listODE[[1]]$x
  if (is.null(t) && input$simulateImmediate && length(GI_imm_list) > 0)     t <- GI_imm_list[[1]]$x
  
  if (is.null(t)) {
    stop("No simulation produced a time vector 't' — check that simulation functions returned results.")
  }
  
  # finalList <- finalSteps(B_imm_list, GI_imm_list, B_sus_list, GI_sus_list, GI_sus_listODE, B_sus_listODE, t)
  finalList <- list(t=t, GI_sus_list = GI_sus_list, B_sus_list = B_sus_list, GI_sus_listODE = GI_sus_listODE, B_sus_listODE = B_sus_listODE, GI_imm_list = GI_imm_list, B_imm_list = B_imm_list,     last_time = if (length(t)>0) t[length(t)] else 0)
  })
  
  # --- GI plot ---
  output$plotGI <- renderPlotly({
    req(input$showGI)
    sim <- simulate_model()
    GIPlotFunc(sim, input)
  })
  
  # --- Blood plot ---
  output$plotBlood <- renderPlotly({
    req(input$showBlood)
    sim <- simulate_model()
    BloodPlotFunc(sim, input)
  })
}

shinyApp(ui = ui, server = server)