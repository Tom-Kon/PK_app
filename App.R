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
source("General parameters.R")
source("Immediate release.R")
source("Sustained release.R")

ui <- fluidPage(
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


    # totals (sum of all per-dose contributions)
    GI_total_vec <- numeric(length(t))
    # combine lists safely: elements of lists are data.frames with $y
    for (lst in c(GI_imm_list, GI_sus_list)) {
      if (length(lst) > 0) {
        # lst is a data.frame element here; sum its y
        GI_total_vec <- GI_total_vec + lst$y
      }
    }
    
    Blood_total_vec <- numeric(length(t))
    for (lst in c(B_imm_list, B_sus_list)) {
      if (length(lst) > 0) {
        Blood_total_vec <- Blood_total_vec + lst$y
      }
    }
    
    
    # per-type totals
    GI_imm_total <- Reduce(`+`, lapply(GI_imm_list, `[[`, "y"), init = numeric(length(t)))
    GI_sus_total <- Reduce(`+`, lapply(GI_sus_list, `[[`, "y"), init = numeric(length(t)))
    B_imm_total  <- Reduce(`+`, lapply(B_imm_list,  `[[`, "y"), init = numeric(length(t)))
    B_sus_total  <- Reduce(`+`, lapply(B_sus_list,  `[[`, "y"), init = numeric(length(t)))
    
    # dynamic trimming
    thr <- 1e-3
    act_idx <- which((GI_total_vec > thr) | (Blood_total_vec > thr))
    if (length(act_idx) == 0) {
      keep_idx <- seq_len(min(50, length(t)))
    } else {
      last_idx <- max(act_idx)
      last_time <- t[last_idx]
      buffer <- max(0.15, 0.02 * last_time)
      keep_idx <- which(t <= (last_time + buffer))
      if (length(keep_idx) == 0) keep_idx <- seq_len(min(50, length(t)))
    }
    
    t_trim <- t[keep_idx]
    GI_total_df    <- data.frame(x = t_trim, y = GI_total_vec[keep_idx], group = "GI total")
    Blood_total_df <- data.frame(x = t_trim, y = Blood_total_vec[keep_idx], group = "Blood total")
    
    GI_imm_list_trim <- lapply(GI_imm_list, function(df) df[df$x <= max(t_trim), , drop = FALSE])
    GI_sus_list_trim <- lapply(GI_sus_list, function(df) df[df$x <= max(t_trim), , drop = FALSE])
    B_imm_list_trim  <- lapply(B_imm_list,  function(df) df[df$x <= max(t_trim), , drop = FALSE])
    B_sus_list_trim  <- lapply(B_sus_list,  function(df) df[df$x <= max(t_trim), , drop = FALSE])
    
    GI_imm_total_df <- data.frame(x = t_trim, y = GI_imm_total[keep_idx], group = "Immediate total")
    GI_sus_total_df <- data.frame(x = t_trim, y = GI_sus_total[keep_idx], group = "Sustained total")
    B_imm_total_df  <- data.frame(x = t_trim, y = B_imm_total[keep_idx], group = "Immediate total")
    B_sus_total_df  <- data.frame(x = t_trim, y = B_sus_total[keep_idx], group = "Sustained total")
    
    
    list(
      t = t_trim,
      GI_total = GI_total_df,
      Blood_total = Blood_total_df,
      GI_imm_list = GI_imm_list_trim,
      GI_sus_list = GI_sus_list_trim,
      B_imm_list = B_imm_list_trim,
      B_sus_list = B_sus_list_trim,
      GI_imm_total = GI_imm_total_df,
      GI_sus_total = GI_sus_total_df,
      B_imm_total = B_imm_total_df,
      B_sus_total = B_sus_total_df,
      z = z,
      last_time = max(t_trim)
    )
  })
  
  # --- GI plot ---
  output$plotGI <- renderPlotly({
    req(input$showGI)
    sim <- simulate_model()
    separate <- isTRUE(input$separateColors)
    show_imm <- input$GIview %in% c("immediate", "both")
    show_sus <- input$GIview %in% c("sustained", "both")
    
    p <- plot_ly()
    # color palettes
    nI <- length(sim$GI_imm_list); colsI <- if (nI>0) palette_hcl(nI, h = c(200,500)) else character(0)
    nS <- length(sim$GI_sus_list); colsS <- if (nS>0) palette_hcl(nS, h = c(0,140)) else character(0)
    
    if (separate) {
      if (show_sus && nS>0) {
        for (j in seq_len(nS)) {
          df <- sim$GI_sus_list[[j]]
          p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines",
                         name = paste0("Sustained ", j), legendgroup = "Sustained",
                         line = list(color = colsS[j]), fill = "none")
        }
      }
      if (show_imm && nI>0) {
        for (j in seq_len(nI)) {
          df <- sim$GI_imm_list[[j]]
          p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines",
                         name = paste0("Immediate ", j), legendgroup = "Immediate",
                         line = list(color = colsI[j]), fill = "none")
        }
      }
    } else {
      #color graph immediate
      if (input$GIview == "immediate") {
        if (nI > 0) {
          p <- add_trace(p, x = sim$GI_imm_total$x, y = sim$GI_imm_total$y, type = "scatter", mode = "lines",
                         name = "Immediate (total)", fill = "none", line = list(color = "darkorange"))
        } else {
          p <- add_trace(p, x = sim$GI_total$x, y = sim$GI_total$y, type = "scatter", mode = "lines",
                         name = "GI (total)", fill = "none")
        }
        #color graph sustained
      } else if (input$GIview == "sustained") {
        if (nS > 0) {
          p <- add_trace(p, x = sim$GI_sus_total$x, y = sim$GI_sus_total$y, type = "scatter", mode = "lines",
                         name = "Sustained (total)", fill = "none", line = list(color = "steelblue"))
        } else {
          p <- add_trace(p, x = sim$GI_total$x, y = sim$GI_total$y, type = "scatter", mode = "lines",
                         name = "GI (total)", fill = "none")
        }
        # both
      } else {
        p <- add_trace(p, x = sim$GI_sus_total$x, y = sim$GI_sus_total$y, type = "scatter", mode = "lines",
                       name = "Sustained (total)", fill = "none", line = list(color = "steelblue"))
        p <- add_trace(p, x = sim$GI_imm_total$x, y = sim$GI_imm_total$y, type = "scatter", mode = "lines",
                       name = "Immediate (total)", fill = "none", line = list(color = "darkorange"))
      }
    }
    
    x_rng <- c(0, sim$last_time)
    p %>% layout(title = "GI Concentration",
                 xaxis = list(title = "Time (h)", range = x_rng),
                 yaxis = list(title = "Conc. (GI)"))
  })
  
  # --- Blood plot ---
  output$plotBlood <- renderPlotly({
    req(input$showBlood)
    sim <- simulate_model()
    separate <- isTRUE(input$separateColors)
    
    p <- plot_ly()
    nI <- length(sim$B_imm_list); colsI <- if (nI>0) palette_hcl(nI, h = c(200,500)) else character(0)
    nS <- length(sim$B_sus_list); colsS <- if (nS>0) palette_hcl(nS, h = c(0,140)) else character(0)
    
    if (separate) {
      if (length(sim$B_sus_list) > 0) {
        for (j in seq_along(sim$B_sus_list)) {
          df <- sim$B_sus_list[[j]]
          p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines",
                         name = paste0("Sustained ", j, " (blood)"),
                         legendgroup = "Sustained", line = list(color = colsS[j]), fill = "none")
        }
      }
      if (length(sim$B_imm_list) > 0) {
        for (j in seq_along(sim$B_imm_list)) {
          df <- sim$B_imm_list[[j]]
          p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines",
                         name = paste0("Immediate ", j, " (blood)"),
                         legendgroup = "Immediate", line = list(color = colsI[j]), fill = "none")
        }
      }
    } else {
      # Color graph combined (blood)
      if (input$bloodMode == "combined") {
        p <- add_trace(p, x = sim$Blood_total$x, y = sim$Blood_total$y, type = "scatter", mode = "lines",
                       name = "Blood (combined)", fill = "none", line = list(color = "red"))
      } else {
        # Color graph seperate (blood)
        p <- add_trace(p, x = sim$B_sus_total$x, y = sim$B_sus_total$y, type = "scatter", mode = "lines",
                       name = "Blood (sustained)", legendgroup = "Sustained", fill = "none", line = list(color = "steelblue"))
        p <- add_trace(p, x = sim$B_imm_total$x, y = sim$B_imm_total$y, type = "scatter", mode = "lines",
                       name = "Blood (immediate)", legendgroup = "Immediate", fill = "none", line = list(color = "darkorange"))
      }
    }
    
    x_rng <- c(0, sim$last_time)
    p %>% layout(title = "Blood Concentration",
                 xaxis = list(title = "Time (h)", range = x_rng),
                 yaxis = list(title = "Conc. (Blood)"))
  })
}

shinyApp(ui = ui, server = server)
