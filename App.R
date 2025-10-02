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
source("Additions and final steps.R")
source("Plots.R")

ui <- fluidPage(
  withMathJax(),   # <---- This enables LaTeX rendering
  UIFunc()
)

server <- function(input, output, session) {
  simulate_model <- reactive({
    
  req(input$simulateImmediate || input$simulateSustained)
  GI_sus_list <- list()
  B_sus_list <- list()
  GI_imm_list <- list()
  B_imm_list <- list()
  susResults <- list()
  immResults <- list()
  
  if(input$simulateSustained) {
    susResults <- SustFunction(input)
    GI_sus_list <- susResults$GI_sus_list
    B_sus_list <- susResults$B_sus_list
    t <- susResults$t
    z <- susResults$z
  }
  
  if(input$simulateImmediate){
    immResults <- ImmFunction(input)
    GI_imm_list <- immResults$GI_imm_list
    B_imm_list <- immResults$B_imm_list
    t <- immResults$t
    z <- immResults$z
  } 
  
  finalList <- finalSteps(B_imm_list, GI_imm_list, B_sus_list, GI_sus_list, t, z)
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