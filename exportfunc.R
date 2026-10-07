# ============================================================
# STATIC EXPORT FUNCTIONS
# ============================================================

GIExportFunc <- function(sim, input) {
  
  separate <- isTRUE(input$separateColors)
  show_imm <- input$GIview %in% c("immediate", "both")
  show_sus <- input$GIview %in% c("sustained", "both")
  
  plot_data <- list()
  
  nI <- length(sim$GI_imm_list)
  nS <- length(sim$GI_sus_list)
  
  colsI <- if (nI > 0) palette_hcl(nI, h = c(200, 500)) else character(0)
  colsS <- if (nS > 0) palette_hcl(nS, h = c(0, 140)) else character(0)
  
  if (separate) {
    
    if (show_sus && nS > 0) {
      for (j in seq_len(nS)) {
        df <- sim$GI_sus_list[[j]]
        plot_data[[length(plot_data) + 1]] <- data.frame(
          x = df$x,
          y = df$y,
          group = paste0("Sustained ", j),
          type = "Sustained",
          color = colsS[j]
        )
      }
    }
    
    if (show_imm && nI > 0) {
      for (j in seq_len(nI)) {
        df <- sim$GI_imm_list[[j]]
        plot_data[[length(plot_data) + 1]] <- data.frame(
          x = df$x,
          y = df$y,
          group = paste0("Immediate ", j),
          type = "Immediate",
          color = colsI[j]
        )
      }
    }
    
  } else {
    
    if (input$GIview == "immediate") {
      
      if (nI > 0) {
        df <- sim$GI_imm_total
        
        plot_data[[1]] <- data.frame(
          x = df$x,
          y = df$y,
          group = "Immediate release",
          type = "Immediate",
          color = "darkorange"
        )
        
      } else {
        df <- sim$GI_total
        
        plot_data[[1]] <- data.frame(
          x = df$x,
          y = df$y,
          group = "GI (total)",
          type = "Total",
          color = "black"
        )
      }
      
    } else if (input$GIview == "sustained") {
      
      if (nS > 0) {
        df <- sim$GI_sus_total
        
        plot_data[[1]] <- data.frame(
          x = df$x,
          y = df$y,
          group = "Sustained release",
          type = "Sustained",
          color = "steelblue"
        )
        
      } else {
        df <- sim$GI_total
        
        plot_data[[1]] <- data.frame(
          x = df$x,
          y = df$y,
          group = "GI (total)",
          type = "Total",
          color = "black"
        )
      }
      
    } else {
      
      df <- sim$GI_sus_total
      
      plot_data[[1]] <- data.frame(
        x = df$x,
        y = df$y,
        group = "Sustained release",
        type = "Sustained",
        color = "steelblue"
      )
      
      df <- sim$GI_imm_total
      
      plot_data[[2]] <- data.frame(
        x = df$x,
        y = df$y,
        group = "Immediate release",
        type = "Immediate",
        color = "darkorange"
      )
    }
  }
  
  df_all <- do.call(rbind, plot_data)
  
  color_values <- setNames(
    vapply(
      plot_data,
      function(x) x$color[1],
      character(1)
    ),
    vapply(
      plot_data,
      function(x) x$group[1],
      character(1)
    )
  )
  
  ggplot(df_all, aes(x = x, y = y, color = group, group = group)) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = color_values) +
    labs(
      title = "API concentration in the gastrointestinal tract",
      x = "Time (h)",
      y = "Concentration in GI tract (µg/mL)",
      color = NULL
    ) +
    coord_cartesian(
      xlim = c(0, sim$last_timeGI)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
}


BloodExportFunc <- function(
    sim,
    input,
    therWindMin = input$therWindMin,
    therWindMax = input$therWindMax
) {
  
  separate <- isTRUE(input$separateColors)
  
  plot_data <- list()
  
  nI <- length(sim$B_imm_list)
  nS <- length(sim$B_sus_list)
  
  colsI <- if (nI > 0) palette_hcl(nI, h = c(200, 500)) else character(0)
  colsS <- if (nS > 0) palette_hcl(nS, h = c(0, 140)) else character(0)
  
  if (separate) {
    
    if (nS > 0) {
      for (j in seq_len(nS)) {
        df <- sim$B_sus_list[[j]]
        
        plot_data[[length(plot_data) + 1]] <- data.frame(
          x = df$x,
          y = df$y,
          group = paste0("Sustained ", j, " (blood)"),
          color = colsS[j]
        )
      }
    }
    
    if (nI > 0) {
      for (j in seq_len(nI)) {
        df <- sim$B_imm_list[[j]]
        
        plot_data[[length(plot_data) + 1]] <- data.frame(
          x = df$x,
          y = df$y,
          group = paste0("Immediate ", j, " (blood)"),
          color = colsI[j]
        )
      }
    }
    
  } else {
    
    df <- sim$B_sus_total
    
    plot_data[[1]] <- data.frame(
      x = df$x,
      y = df$y,
      group = "Sustained release",
      color = "steelblue"
    )
    
    df <- sim$B_imm_total
    
    plot_data[[2]] <- data.frame(
      x = df$x,
      y = df$y,
      group = "Immediate release",
      color = "darkorange"
    )
  }
  
  df_all <- do.call(rbind, plot_data)
  
  color_values <- setNames(
    vapply(
      plot_data,
      function(x) x$color[1],
      character(1)
    ),
    vapply(
      plot_data,
      function(x) x$group[1],
      character(1)
    )
  )
  
  p <- ggplot(
    df_all,
    aes(x = x, y = y, color = group, group = group)
  ) +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = color_values) +
    labs(
      title = "API concentration in blood",
      x = "Time (h)",
      y = "Concentration in blood (µg/mL)",
      color = NULL
    ) +
    coord_cartesian(
      xlim = c(0, sim$last_timeBlood)
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right"
    )
  
  # Therapeutic window
  if (isTRUE(input$showTherWind)) {
    
    p <- p +
      geom_hline(
        yintercept = therWindMin,
        linetype = "dotted",
        color = "grey"
      ) +
      geom_hline(
        yintercept = therWindMax,
        linetype = "dotted",
        color = "grey"
      ) +
      annotate(
        "rect",
        xmin = 0,
        xmax = sim$last_timeBlood,
        ymin = therWindMin,
        ymax = therWindMax,
        fill = "grey",
        alpha = 0.2
      )
  }
  
  p
}



download_Excel <- function(sim, input) {
  
  wb <- createWorkbook()
  
  
  # ============================================================
  # GI TRACT
  # ============================================================
  
  addWorksheet(wb, "GI tract")
  
  separate <- isTRUE(input$separateColors)
  show_imm <- input$GIview %in% c("immediate", "both")
  show_sus <- input$GIview %in% c("sustained", "both")
  
  col <- 1
  
  if (separate) {
    
    # ---- Sustained individual curves ----
    if (show_sus && length(sim$GI_sus_list) > 0) {
      
      for (j in seq_along(sim$GI_sus_list)) {
        
        df <- sim$GI_sus_list[[j]]
        
        writeData(
          wb,
          "GI tract",
          data.frame(
            Time_h = df$x,
            Concentration_ug_mL = df$y
          ),
          startCol = col,
          startRow = 2
        )
        
        writeData(
          wb,
          "GI tract",
          paste0("Sustained ", j),
          startCol = col,
          startRow = 1
        )
        
        col <- col + 3
      }
    }
    
    # ---- Immediate individual curves ----
    if (show_imm && length(sim$GI_imm_list) > 0) {
      
      for (j in seq_along(sim$GI_imm_list)) {
        
        df <- sim$GI_imm_list[[j]]
        
        writeData(
          wb,
          "GI tract",
          data.frame(
            Time_h = df$x,
            Concentration_ug_mL = df$y
          ),
          startCol = col,
          startRow = 2
        )
        
        writeData(
          wb,
          "GI tract",
          paste0("Immediate ", j),
          startCol = col,
          startRow = 1
        )
        
        col <- col + 3
      }
    }
    
  } else {
    
    # ---- Same selection as GIPlotFunc ----
    if (input$GIview == "immediate") {
      
      if (length(sim$GI_imm_list) > 0) {
        df <- sim$GI_imm_total
        
        writeData(
          wb,
          "GI tract",
          data.frame(
            Time_h = df$x,
            Concentration_ug_mL = df$y
          ),
          startCol = 1,
          startRow = 2
        )
        
        writeData(
          wb,
          "GI tract",
          "Immediate release",
          startCol = 1,
          startRow = 1
        )
        
      } else {
        df <- sim$GI_total
        
        writeData(
          wb,
          "GI tract",
          data.frame(
            Time_h = df$x,
            Concentration_ug_mL = df$y
          ),
          startCol = 1,
          startRow = 2
        )
        
        writeData(
          wb,
          "GI tract",
          "GI (total)",
          startCol = 1,
          startRow = 1
        )
      }
      
    } else if (input$GIview == "sustained") {
      
      if (length(sim$GI_sus_list) > 0) {
        df <- sim$GI_sus_total
        
        writeData(
          wb,
          "GI tract",
          data.frame(
            Time_h = df$x,
            Concentration_ug_mL = df$y
          ),
          startCol = 1,
          startRow = 2
        )
        
        writeData(
          wb,
          "GI tract",
          "Sustained release",
          startCol = 1,
          startRow = 1
        )
        
      } else {
        df <- sim$GI_total
        
        writeData(
          wb,
          "GI tract",
          data.frame(
            Time_h = df$x,
            Concentration_ug_mL = df$y
          ),
          startCol = 1,
          startRow = 2
        )
        
        writeData(
          wb,
          "GI tract",
          "GI (total)",
          startCol = 1,
          startRow = 1
        )
      }
      
    } else {
      
      # ---- Both ----
      
      df <- sim$GI_sus_total
      
      writeData(
        wb,
        "GI tract",
        data.frame(
          Time_h = df$x,
          Concentration_ug_mL = df$y
        ),
        startCol = 1,
        startRow = 2
      )
      
      writeData(
        wb,
        "GI tract",
        "Sustained release",
        startCol = 1,
        startRow = 1
      )
      
      df <- sim$GI_imm_total
      
      writeData(
        wb,
        "GI tract",
        data.frame(
          Time_h = df$x,
          Concentration_ug_mL = df$y
        ),
        startCol = 4,
        startRow = 2
      )
      
      writeData(
        wb,
        "GI tract",
        "Immediate release",
        startCol = 4,
        startRow = 1
      )
    }
  }
  
  
  # ============================================================
  # BLOOD
  # ============================================================
  
  addWorksheet(wb, "Blood")
  
  separate <- isTRUE(input$separateColors)
  
  col <- 1
  
  if (separate) {
    
    # ---- Sustained individual curves ----
    if (length(sim$B_sus_list) > 0) {
      
      for (j in seq_along(sim$B_sus_list)) {
        
        df <- sim$B_sus_list[[j]]
        
        writeData(
          wb,
          "Blood",
          data.frame(
            Time_h = df$x,
            Concentration_ug_mL = df$y
          ),
          startCol = col,
          startRow = 2
        )
        
        writeData(
          wb,
          "Blood",
          paste0("Sustained ", j, " (blood)"),
          startCol = col,
          startRow = 1
        )
        
        col <- col + 3
      }
    }
    
    # ---- Immediate individual curves ----
    if (length(sim$B_imm_list) > 0) {
      
      for (j in seq_along(sim$B_imm_list)) {
        
        df <- sim$B_imm_list[[j]]
        
        writeData(
          wb,
          "Blood",
          data.frame(
            Time_h = df$x,
            Concentration_ug_mL = df$y
          ),
          startCol = col,
          startRow = 2
        )
        
        writeData(
          wb,
          "Blood",
          paste0("Immediate ", j, " (blood)"),
          startCol = col,
          startRow = 1
        )
        
        col <- col + 3
      }
      
    }
    
  } else {
    
    bloodMode <- if (length(input$bloodMode) > 0) {
      input$bloodMode
    } else {
      "not combined"
    }
    
    if (bloodMode == "combined") {
      
      # Your plot currently has no combined plotting code,
      # so there is nothing to export here yet.
      
    } else {
      
      # ---- Exactly the same two traces as BloodPlotFunc ----
      
      df <- sim$B_sus_total
      
      writeData(
        wb,
        "Blood",
        data.frame(
          Time_h = df$x,
          Concentration_ug_mL = df$y
        ),
        startCol = 1,
        startRow = 2
      )
      
      writeData(
        wb,
        "Blood",
        "Sustained release",
        startCol = 1,
        startRow = 1
      )
      
      df <- sim$B_imm_total
      
      writeData(
        wb,
        "Blood",
        data.frame(
          Time_h = df$x,
          Concentration_ug_mL = df$y
        ),
        startCol = 4,
        startRow = 2
      )
      
      writeData(
        wb,
        "Blood",
        "Immediate release",
        startCol = 4,
        startRow = 1
      )
    }
  }
  
  
  return(wb)
}