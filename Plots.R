GIPlotFunc <- function(sim, input) {
  separate <- isTRUE(input$separateColors)
  show_imm <- input$GIview %in% c("immediate", "both")
  show_sus <- input$GIview %in% c("sustained", "both")
  
  p <- plot_ly()
  # color palettes
  nI <- length(sim$GI_imm_list); colsI <- if (nI>0) palette_hcl(nI, h = c(200,500)) else character(0)
  nS <- length(sim$GI_sus_list); colsS <- if (nS>0) palette_hcl(nS, h = c(0,140)) else character(0)
  nSO <- length(sim$GI_sus_listODE); colsSO <- if (nSO>0) palette_hcl(nSO, h = c(0,140)) else character(0)
  print(show_sus)
  
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
  
  if (show_sus && nSO>0) {
    for (j in seq_len(nSO)) {
      df <- sim$GI_sus_listODE[[j]]
      p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines", 
                     name = paste0("GI (ODE ", j, ")"), legendgroup = "Sustained ODE",
                     line = list(color = colsSO[j]), fill = "none")
    }
  }  
  return(p)
  
}


BloodPlotFunc <- function(sim, input) {
  separate <- isTRUE(input$separateColors)
  
  p <- plot_ly()
  nI <- length(sim$B_imm_list); colsI <- if (nI>0) palette_hcl(nI, h = c(200,500)) else character(0)
  nS <- length(sim$B_sus_list); colsS <- if (nS>0) palette_hcl(nS, h = c(0,140)) else character(0)
  
  if (separate) {
    if (nS > 0) {
      for (j in seq_len(nS)) {
        df <- sim$B_sus_list[[j]]
        p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines",
                       name = paste0("Sustained ", j, " (blood)"),
                       legendgroup = "Sustained", line = list(color = colsS[j]), fill = "none")
      }
    }
    if (nI > 0) {
      for (j in seq_len(nI)) {
        df <- sim$B_imm_list[[j]]
        p <- add_trace(p, x = df$x, y = df$y, type = "scatter", mode = "lines",
                       name = paste0("Immediate ", j, " (blood)"),
                       legendgroup = "Immediate", line = list(color = colsI[j]), fill = "none")
      }
    }
  }
  
  # --- add ODE overlay ---
  if (!is.null(sim$B_sus_listODE) && length(sim$B_sus_listODE) > 0) {
    for (j in seq_along(sim$B_sus_listODE)) {
      df <- sim$B_sus_listODE[[j]]
      p <- add_trace(p, x = df$x, y = df$y,
                     type = "scatter", mode = "lines", name = paste0("Blood (ODE ", j, ")"),
                     line = list(color = "black", dash = "dot"), fill = "none")
    }
  }
  
  x_rng <- c(0, sim$last_time)
  p %>% layout(title = "Blood Concentration",
               xaxis = list(title = "Time (h)", range = x_rng),
               yaxis = list(title = "Conc. (Blood)"))
}
