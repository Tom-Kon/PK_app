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
    if (input$GIview == "immediate") {
      if (nI > 0) {
        p <- add_trace(p, x = sim$GI_imm_total$x, y = sim$GI_imm_total$y,
                       type = "scatter", mode = "lines", name = "Immediate (total)",
                       fill = "none", line = list(color = "darkorange"))
      } else {
        p <- add_trace(p, x = sim$GI_total$x, y = sim$GI_total$y,
                       type = "scatter", mode = "lines", name = "GI (total)", fill = "none")
      }
    } else if (input$GIview == "sustained") {
      if (nS > 0) {
        p <- add_trace(p, x = sim$GI_sus_total$x, y = sim$GI_sus_total$y,
                       type = "scatter", mode = "lines", name = "Sustained (total)",
                       fill = "none", line = list(color = "steelblue"))
      } else {
        p <- add_trace(p, x = sim$GI_total$x, y = sim$GI_total$y,
                       type = "scatter", mode = "lines", name = "GI (total)", fill = "none")
      }
    } else {
      p <- add_trace(p, x = sim$GI_sus_total$x, y = sim$GI_sus_total$y,
                     type = "scatter", mode = "lines", name = "Sustained (total)",
                     fill = "none", line = list(color = "steelblue"))
      p <- add_trace(p, x = sim$GI_imm_total$x, y = sim$GI_imm_total$y,
                     type = "scatter", mode = "lines", name = "Immediate (total)",
                     fill = "none", line = list(color = "darkorange"))
    }
  }
  
  # --- add ODE overlay ---
  if (!is.null(sim$GI_sus_listODE)) {
    p <- add_trace(p, x = sim$GI_sus_listODE$x, y = sim$GI_sus_listODE$y,
                   type = "scatter", mode = "lines", name = "GI (ODE)",
                   line = list(color = "black", dash = "dot"), fill = "none")
  }
  
  x_rng <- c(0, sim$last_time)
  p %>% layout(title = "GI Concentration",
               xaxis = list(title = "Time (h)", range = x_rng),
               yaxis = list(title = "Conc. (GI)"))
}

BloodPlotFunc <- function(sim, input) {
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
    if (input$bloodMode == "combined") {
      p <- add_trace(p, x = sim$Blood_total$x, y = sim$Blood_total$y,
                     type = "scatter", mode = "lines", name = "Blood (combined)",
                     fill = "none", line = list(color = "red"))
    } else {
      p <- add_trace(p, x = sim$B_sus_total$x, y = sim$B_sus_total$y,
                     type = "scatter", mode = "lines", name = "Blood (sustained)",
                     legendgroup = "Sustained", fill = "none", line = list(color = "steelblue"))
      p <- add_trace(p, x = sim$B_imm_total$x, y = sim$B_imm_total$y,
                     type = "scatter", mode = "lines", name = "Blood (immediate)",
                     legendgroup = "Immediate", fill = "none", line = list(color = "darkorange"))
    }
  }
  
  # --- add ODE overlay ---
  if (!is.null(sim$B_sus_listODE)) {
    p <- add_trace(p, x = sim$B_sus_listODE$x, y = sim$B_sus_listODE$y,
                   type = "scatter", mode = "lines", name = "Blood (ODE)",
                   line = list(color = "black", dash = "dot"), fill = "none")
  }
  
  x_rng <- c(0, sim$last_time)
  p %>% layout(title = "Blood Concentration",
               xaxis = list(title = "Time (h)", range = x_rng),
               yaxis = list(title = "Conc. (Blood)"))
}
