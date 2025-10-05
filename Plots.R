GIPlotFunc <- function(sim, input) {
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
                       name = "Immediate release", fill = "none", line = list(color = "darkorange"))
      } else {
        p <- add_trace(p, x = sim$GI_total$x, y = sim$GI_total$y, type = "scatter", mode = "lines",
                       name = "GI (total)", fill = "none")
      }
      #color graph sustained
    } else if (input$GIview == "sustained") {
      if (nS > 0) {
        p <- add_trace(p, x = sim$GI_sus_total$x, y = sim$GI_sus_total$y, type = "scatter", mode = "lines",
                       name = "Sustained release", fill = "none", line = list(color = "steelblue"))
      } else {
        p <- add_trace(p, x = sim$GI_total$x, y = sim$GI_total$y, type = "scatter", mode = "lines",
                       name = "GI (total)", fill = "none")
      }
      # both
    } else {
      p <- add_trace(p, x = sim$GI_sus_total$x, y = sim$GI_sus_total$y, type = "scatter", mode = "lines",
                     name = "Sustained release", fill = "none", line = list(color = "steelblue"))
      p <- add_trace(p, x = sim$GI_imm_total$x, y = sim$GI_imm_total$y, type = "scatter", mode = "lines",
                     name = "Immediate release", fill = "none", line = list(color = "darkorange"))
    }
  }
  
  x_rng <- c(0, sim$last_timeGI)
  p %>% layout(title = "GI Concentration",
               xaxis = list(title = "Time (h)", range = x_rng),
               yaxis = list(title = "Concentration in GI tract (mg/mL)"))
}


BloodPlotFunc <- function(sim, input) {
  separate <- isTRUE(input$separateColors)
  bloodMode <- if (length(input$bloodMode) > 0) input$bloodMode else "not combined"
  
  
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
    if (bloodMode == "combined") {
      # p <- add_trace(p, x = sim$Blood_total$x, y = sim$Blood_total$y, type = "scatter", mode = "lines",
      #                name = "Blood (combined)", fill = "none", line = list(color = "red"))
    } else {
      # Color graph separate (blood)
      p <- add_trace(p, x = sim$B_sus_total$x, y = sim$B_sus_total$y, type = "scatter", mode = "lines",
                     name = "Sustained release", legendgroup = "Sustained", fill = "none", line = list(color = "steelblue"))
      p <- add_trace(p, x = sim$B_imm_total$x, y = sim$B_imm_total$y, type = "scatter", mode = "lines",
                     name = "Immediate release", legendgroup = "Immediate", fill = "none", line = list(color = "darkorange"))
    }
  }
  
  x_rng <- c(0, sim$last_timeBlood)
  p %>% layout(title = "Blood Concentration",
               xaxis = list(title = "Time (h)", range = x_rng),
               yaxis = list(title = "Concentration in blood (mg/mL)"))
}
  