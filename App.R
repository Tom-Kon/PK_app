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
source("exportfunc.R")

custom_theme <- bs_theme(
  version = 5,
  preset = "lumen",
)

ui <- UIFunc(custom_theme)

server <- function(input, output, session) {
  observeEvent(
    list(
      input$ClVdt0.5Fix,
      input$Vd,
      input$Cl,
      input$t0.5,
      input$weight
    ), {
    fixed <- input$ClVdt0.5Fix
    generalList <- generalParams(input)
    list2env(generalList, envir = environment())

    if (fixed == "t0.5Fix") {
      shinyjs::disable("t0.5")
      shinyjs::enable("Vd")
      shinyjs::enable("Cl")
      
    } else if (fixed == "VdFix") {
      shinyjs::disable("Vd")
      shinyjs::enable("t0.5")
      shinyjs::enable("Cl")
      
    } else if (fixed == "ClFix") {
      shinyjs::disable("Cl")
      shinyjs::enable("t0.5")
      shinyjs::enable("Vd")
    }
    
    if (fixed == "t0.5Fix") {
      
      updateSliderInput(
        session,
        "t0.5",
        min = min(0.2, t0.5_UI),
        max = max(40, t0.5_UI),
        value = t0.5_UI
      )

    } else if (fixed == "VdFix") {
      
      updateSliderInput(
        session,
        "Vd",
        min = min(0.01, Vd_UI),
        max = max(3, Vd_UI),
        value = Vd_UI
      )
      
    } else {
      
      updateSliderInput(
        session,
        "Cl",
        min = min(0.5, Cl_UI),
        max = max(20, Cl_UI),
        value = Cl_UI
      )

    }
    
  })
  

  
  
  simulate_model <- reactive({
    
    
    req(input$D_Imm)
    req(input$sust_dose)
    
    req(input$simulateImmediate || input$simulateSustained)
    
    GI_sus_list <- list()
    B_sus_list <- list()
    GI_imm_list <- list()
    B_imm_list <- list()
    susResults <- list()
    immResults <- list()
    
    if (input$simulateSustained) {
      susResults <- SustFunction(input)
      GI_sus_list <- susResults$GI_sus_list
      B_sus_list <- susResults$B_sus_list
      t <- susResults$t
    }
    
    if (input$simulateImmediate) {
      immResults <- ImmFunction(input)
      GI_imm_list <- immResults$GI_imm_list
      B_imm_list <- immResults$B_imm_list
      t <- immResults$t
    }
    
    finalList <- finalSteps(
      B_imm_list,
      GI_imm_list,
      B_sus_list,
      GI_sus_list,
      t, 
      input
    )
    
  })
  
  
  # ============================================================
  # Output Parameters
  # ============================================================
  observe({
    req(simulate_model())
    sim <- simulate_model()
    
    finalList <- sim$ParamList
    
    outputList <- outputGenerator(input, finalList)

    if (input$simulateSustained) {
      output$SusParameters <- renderText({
        outputList$textSus
      })
    }
    
    if (input$simulateImmediate) {
      output$ImmParameters <- renderText({
        outputList$textImm
      })
    }
  })
  
  
  # ============================================================
  # INTERACTIVE PLOTS
  # ============================================================
  
  output$plotGI <- renderPlotly({
    req(input$showGI)
    
    sim <- simulate_model()
    
    GIPlotFunc(sim, input)
  })
  
  
  output$plotBlood <- renderPlotly({
    req(input$showBlood)
    
    sim <- simulate_model()
    
    BloodPlotFunc(sim, input)
  })
  
  
  # ============================================================
  # DOWNLOAD GI
  # ============================================================
  
  output$downloadGI <- downloadHandler(
    
    filename = function() {
      paste0(
        Sys.Date(),
        "_GI tract simulation.tiff"
      )
    },
    
    content = function(file) {
      
      sim <- simulate_model()
      
      p <- GIExportFunc(sim, input)
      
      ggsave(
        filename = file,
        plot = p,
        device = "tiff",
        dpi = 600,
        width = 30,
        height = 20,
        units = "cm",
        compression = "lzw"
      )
    }
  )
  
  
  # ============================================================
  # DOWNLOAD BLOOD
  # ============================================================
  
  output$downloadBlood <- downloadHandler(
    
    filename = function() {
      paste0(
        Sys.Date(),
        "_plasma simulation.tiff"
      )
    },
    
    content = function(file) {
      
      sim <- simulate_model()
      
      p <- BloodExportFunc(sim, input)
      
      ggsave(
        filename = file,
        plot = p,
        device = "tiff",
        dpi = 600,
        width = 30,
        height = 20,
        units = "cm",
        compression = "lzw"
      )
    }
  )
  
  # DOWNLOAD EXCEL
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_drug release simulation.xlsx")
    },
    content = function(file) {
      
      sim <- simulate_model()
      
      wb <- download_Excel(sim, input)
      
      openxlsx::saveWorkbook(
        wb,
        file = file,
        overwrite = TRUE
      )
    }
  )
    
}
shinyApp(ui = ui, server = server)